/* Functions for generic NeXT as target machine for GNU C compiler.
   Copyright (C) 1989, 1990, 1991, 1992, 1993, 1996, 1997, 1998,
   2000, 2002 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "flags.h"
#include "tree.h"
#include "rtl.h"
#include "toplev.h"
#include "output.h"
#include "tm_p.h"

/* Make everything that used to go in the text section really go there.  */

int flag_no_mach_text_sections = 0;

#define OPT_STRCMP(opt) (!strncmp (opt, p, sizeof (opt)-1))

/* 1 if handle_pragma has been called yet.  */

static int pragma_initialized;

/* Initial setting of `optimize'.  */

static int initial_optimize_flag;

/* Called from check_newline via the macro HANDLE_PRAGMA.
   FINPUT is the source file input stream.
   CH is the first character after `#pragma'.
   The result is 1 if the pragma was handled.  */

int
handle_pragma (p_getc, p_ungetc, pname)
     int (*  p_getc) PARAMS ((void)) ATTRIBUTE_UNUSED;
     void (* p_ungetc) PARAMS ((int)) ATTRIBUTE_UNUSED;
     const char *pname;
{
  int retval = 0;

  /* Record initial setting of optimize flag, so we can restore it.  */
  if (!pragma_initialized)
    {
      pragma_initialized = 1;
      initial_optimize_flag = optimize;
    }

  if (strcmp (pname, "CC_OPT_ON") == 0)
    {
      optimize = 1;
      warning ("optimization turned on");
      retval = 1;
    }
  else if (strcmp (pname, "CC_OPT_OFF") == 0)
    {
      optimize = 0;
      warning ("optimization turned off");
      retval = 1;
    }
  else if (strcmp (pname, "CC_OPT_RESTORE") == 0)
    {
      extern int initial_optimize_flag;

      if (optimize != initial_optimize_flag)
	optimize = initial_optimize_flag;
      warning ("optimization level restored");
      retval = 1;
    }
  else if (strcmp (pname, "CC_WRITABLE_STRINGS") == 0)
    flag_writable_strings = retval = 1;
  else if (strcmp (pname, "CC_NON_WRITABLE_STRINGS") == 0)
    flag_writable_strings = 0, retval = 1;
  else if (strcmp (pname, "CC_NO_MACH_TEXT_SECTIONS") == 0)
    flag_no_mach_text_sections = retval = 1;

  return retval;
}

void
nextstep_asm_out_constructor (symbol, priority)
     rtx symbol;
     int priority ATTRIBUTE_UNUSED;
{
  constructor_section ();
  assemble_align (POINTER_SIZE);
  assemble_integer (symbol, POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);
  fprintf (asm_out_file, ".reference .constructors_used\n");
}

void
nextstep_asm_out_destructor (symbol, priority)
     rtx symbol;
     int priority ATTRIBUTE_UNUSED;
{
  destructor_section ();
  assemble_align (POINTER_SIZE);
  assemble_integer (symbol, POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);
  fprintf (asm_out_file, ".reference .destructors_used\n");
}

void
nextstep_select_section (exp, reloc, align)
     tree exp;
     int reloc;
     unsigned HOST_WIDE_INT align ATTRIBUTE_UNUSED;
{
  if (TREE_CODE (exp) == STRING_CST)
    {
      if (flag_writable_strings)
	data_section ();
      else if (TREE_STRING_LENGTH (exp)
	       != strlen (TREE_STRING_POINTER (exp)) + 1)
	readonly_data_section ();
      else
	cstring_section ();
    }
  else if (TREE_CODE (exp) == INTEGER_CST
	   || TREE_CODE (exp) == REAL_CST)
    {
      tree size = TYPE_SIZE (TREE_TYPE (exp));
      HOST_WIDE_INT size_int;

      if (TREE_CODE (size) == INTEGER_CST)
	size_int = tree_low_cst (size, 1);
      else
	size_int = 0;

      if (size_int == 4)
	literal4_section ();
      else if (size_int == 8)
	literal8_section ();
      else
	readonly_data_section ();
    }
  else if (TREE_CODE (exp) == CONSTRUCTOR
	   && TREE_TYPE (exp)
	   && TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE
	   && TYPE_NAME (TREE_TYPE (exp))
	   && TREE_CODE (TYPE_NAME (TREE_TYPE (exp))) == IDENTIFIER_NODE
	   && IDENTIFIER_POINTER (TYPE_NAME (TREE_TYPE (exp))))
    {
      if (!strcmp (IDENTIFIER_POINTER (TYPE_NAME (TREE_TYPE (exp))),
		   "NXConstantString"))
	objc_string_object_section ();
      else if ((TREE_READONLY (exp) || TREE_CONSTANT (exp))
	       && !TREE_SIDE_EFFECTS (exp))
	readonly_data_section ();
      else
	data_section ();
    }
  else if (TREE_CODE (exp) == VAR_DECL
	   && DECL_NAME (exp)
	   && TREE_CODE (DECL_NAME (exp)) == IDENTIFIER_NODE
	   && IDENTIFIER_POINTER (DECL_NAME (exp))
	   && !strncmp (IDENTIFIER_POINTER (DECL_NAME (exp)), "_OBJC_", 6))
    {
      const char *name = IDENTIFIER_POINTER (DECL_NAME (exp));

      if (!strncmp (name, "_OBJC_CLASS_METHODS_", 20))
	objc_cls_meth_section ();
      else if (!strncmp (name, "_OBJC_INSTANCE_METHODS_", 23))
	objc_inst_meth_section ();
      else if (!strncmp (name, "_OBJC_CATEGORY_CLASS_METHODS_", 20))
	objc_cat_cls_meth_section ();
      else if (!strncmp (name, "_OBJC_CATEGORY_INSTANCE_METHODS_", 23))
	objc_cat_inst_meth_section ();
      else if (!strncmp (name, "_OBJC_CLASS_VARIABLES_", 22))
	objc_class_vars_section ();
      else if (!strncmp (name, "_OBJC_INSTANCE_VARIABLES_", 25))
	objc_instance_vars_section ();
      else if (!strncmp (name, "_OBJC_CLASS_PROTOCOLS_", 22))
	objc_cat_cls_meth_section ();
      else if (!strncmp (name, "_OBJC_CLASS_NAME_", 17))
	objc_class_names_section ();
      else if (!strncmp (name, "_OBJC_METH_VAR_NAME_", 20))
	objc_meth_var_names_section ();
      else if (!strncmp (name, "_OBJC_METH_VAR_TYPE_", 20))
	objc_meth_var_types_section ();
      else if (!strncmp (name, "_OBJC_CLASS_REFERENCES", 22))
	objc_cls_refs_section ();
      else if (!strncmp (name, "_OBJC_CLASS_", 12))
	objc_class_section ();
      else if (!strncmp (name, "_OBJC_METACLASS_", 16))
	objc_meta_class_section ();
      else if (!strncmp (name, "_OBJC_CATEGORY_", 15))
	objc_category_section ();
      else if (!strncmp (name, "_OBJC_SELECTOR_REFERENCES", 25))
	objc_selector_refs_section ();
      else if (!strncmp (name, "_OBJC_SYMBOLS", 13))
	objc_symbols_section ();
      else if (!strncmp (name, "_OBJC_MODULES", 13))
	objc_module_info_section ();
      else if (!strncmp (name, "_OBJC_PROTOCOL_INSTANCE_METHODS_", 32))
	objc_cat_inst_meth_section ();
      else if (!strncmp (name, "_OBJC_PROTOCOL_CLASS_METHODS_", 29))
	objc_cat_cls_meth_section ();
      else if (!strncmp (name, "_OBJC_PROTOCOL_REFS_", 20))
	objc_cat_cls_meth_section ();
      else if (!strncmp (name, "_OBJC_PROTOCOL_", 15))
	objc_protocol_section ();
      else if ((TREE_READONLY (exp) || TREE_CONSTANT (exp))
	       && !TREE_SIDE_EFFECTS (exp))
	readonly_data_section ();
      else
	data_section ();
    }
  else if (TREE_CODE (exp) == VAR_DECL)
    {
      if ((flag_pic && reloc)
	  || !TREE_READONLY (exp) || TREE_SIDE_EFFECTS (exp)
	  || !DECL_INITIAL (exp)
	  || (DECL_INITIAL (exp) != error_mark_node
	      && !TREE_CONSTANT (DECL_INITIAL (exp))))
	data_section ();
      else
	readonly_data_section ();
    }
  else
    readonly_data_section ();
}

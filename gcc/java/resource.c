/* Functions related to building resource files.
   Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "flags.h"
#include "java-tree.h"
#include "jcf.h"
#include "obstack.h"
#include "toplev.h"
#include "output.h"
#include "parse.h"
#include "function.h"
#include "ggc.h"
#include "stdio.h"
#include "target.h"
#include "expr.h"

/* DOS brain-damage */
#ifndef O_BINARY
#define O_BINARY 0 /* MS-DOS brain-damage */
#endif

/* A list of all the resources files.  */
static GTY(()) tree resources = NULL;

/* Function used to register resources.  */
static GTY(()) rtx registerResource_libfunc;

/* Count of all the resources compiled in this invocation.  */
static int Jr_count = 0;

void
compile_resource_data (const char *name, const char *buffer, int length)
{
  tree rtype, field = NULL_TREE, data_type, rinit, data, decl;
  char buf[60];

  data_type = build_prim_array_type (unsigned_byte_type_node,
				     strlen (name) + length);
  rtype = make_node (RECORD_TYPE);
  PUSH_FIELD (rtype, field, "name_length", unsigned_int_type_node);
  PUSH_FIELD (rtype, field, "resource_length", unsigned_int_type_node);
  PUSH_FIELD (rtype, field, "data", data_type);
  FINISH_RECORD (rtype);
  START_RECORD_CONSTRUCTOR (rinit, rtype);
  PUSH_FIELD_VALUE (rinit, "name_length", 
		    build_int_2 (strlen (name), 0));
  PUSH_FIELD_VALUE (rinit, "resource_length", 
		    build_int_2 (length, 0));
  data = build_string (strlen(name) + length, buffer);
  TREE_TYPE (data) = data_type;
  PUSH_FIELD_VALUE (rinit, "data", data);
  FINISH_RECORD_CONSTRUCTOR (rinit);
  TREE_CONSTANT (rinit) = 1;

  /* Generate a unique-enough identifier.  */
  sprintf (buf, "_Jr%d", ++Jr_count);

  decl = build_decl (VAR_DECL, get_identifier (buf), rtype);
  TREE_STATIC (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  TREE_READONLY (decl) = 1;
  TREE_THIS_VOLATILE (decl) = 0;
  DECL_INITIAL (decl) = rinit;
  layout_decl (decl, 0);
  pushdecl (decl);
  rest_of_decl_compilation (decl, (char*) 0, global_bindings_p (), 0);
  make_decl_rtl (decl, (char*) 0);
  assemble_variable (decl, 1, 0, 0);

  resources = tree_cons (NULL_TREE, decl, resources);
}

void
write_resource_constructor (void)
{
  tree init_name, init_type, init_decl;
  tree iter;
  location_t saved_loc = input_location;
  char *resource_ctor_name;

  /* Only do work if required.  */
  if (resources == NULL_TREE)
    return;

  resource_ctor_name = concat (IDENTIFIER_POINTER (get_file_function_name ('I')),
			       "_resource", NULL);
  init_name = get_identifier (resource_ctor_name);
  free (resource_ctor_name);
  init_type = build_function_type (void_type_node, end_params_node);

  init_decl = build_decl (FUNCTION_DECL, init_name, init_type);
  DECL_SOURCE_LINE (init_decl) = 0;
  SET_DECL_ASSEMBLER_NAME (init_decl, init_name);
  TREE_STATIC (init_decl) = 1;
  current_function_decl = init_decl;
  DECL_RESULT (init_decl) = build_decl (RESULT_DECL, 
					NULL_TREE, void_type_node);

  /* It can be a static function as long as collect2 does not have
     to scan the object file to find its ctor/dtor routine.  */
  TREE_PUBLIC (init_decl) = ! targetm.have_ctors_dtors;

  pushlevel (0);
  make_decl_rtl (init_decl, NULL);
  init_function_start (init_decl);
  expand_function_start (init_decl, 0);

  /* Write out entries in the same order in which they were defined.  */
  for (iter = nreverse (resources); iter != NULL_TREE;
       iter = TREE_CHAIN (iter))
    {
      emit_library_call (registerResource_libfunc, 0, VOIDmode, 1,
			 expand_expr (build_address_of (TREE_VALUE (iter)),
				      0, Pmode, 0),
			 Pmode);
    }

  input_location = DECL_SOURCE_LOCATION (init_decl);
  expand_function_end ();
  poplevel (1, 0, 1);
  { 
    /* Force generation, even with -O3 or deeper.  Gross hack.
       FIXME.  */
    int saved_flag = flag_inline_functions;
    flag_inline_functions = 0;	
    rest_of_compilation (init_decl);
    flag_inline_functions = saved_flag;
  }
  current_function_decl = NULL_TREE;
  (* targetm.asm_out.constructor) (XEXP (DECL_RTL (init_decl), 0),
				   DEFAULT_INIT_PRIORITY);
  input_location = saved_loc;
}

/* Generate a byte array representing the contents of FILENAME.  The
   array is assigned a unique local symbol.  The array represents a
   compiled Java resource, which is accessed by the runtime using
   NAME.  */
void
compile_resource_file (const char *name, const char *filename)
{
  struct stat stat_buf;
  int fd;
  char *buffer;

  fd = open (filename, O_RDONLY | O_BINARY);
  if (fd < 0)
    {
      perror ("Failed to read resource file");
      return;
    }
  if (fstat (fd, &stat_buf) != 0
      || ! S_ISREG (stat_buf.st_mode))
    {
      perror ("Could not figure length of resource file");
      return;
    }
  buffer = xmalloc (strlen (name) + stat_buf.st_size);
  strcpy (buffer, name);
  read (fd, buffer + strlen (name), stat_buf.st_size);
  close (fd);

  compile_resource_data (name, buffer, stat_buf.st_size);
  write_resource_constructor ();
}

void
init_resource_processing (void)
{
  registerResource_libfunc =
    gen_rtx_SYMBOL_REF (Pmode, "_Jv_RegisterResource");
}

#include "gt-java-resource.h"

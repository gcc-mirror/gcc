/* Do-nothing debug hooks for GCC.
   Copyright (C) 2001-2023 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "debug.h"

/* The do-nothing debug hooks.  */
const struct gcc_debug_hooks do_nothing_debug_hooks =
{
  debug_nothing_charstar,
  debug_nothing_charstar,
  debug_nothing_charstar,			/* early_finish */
  debug_nothing_void,
  debug_nothing_int_charstar,
  debug_nothing_int_charstar,
  debug_nothing_int_charstar,
  debug_nothing_int,
  debug_nothing_int_int,	         /* begin_block */
  debug_nothing_int_int,	         /* end_block */
  debug_true_const_tree,	         /* ignore_block */
  debug_nothing_int_int_charstar_int_bool, /* source_line */
  debug_nothing_int_int_charstar,	 /* set_ignored_loc */
  debug_nothing_int_int_charstar,	 /* begin_prologue */
  debug_nothing_int_charstar,	         /* end_prologue */
  debug_nothing_int_charstar,	         /* begin_epilogue */
  debug_nothing_int_charstar,	         /* end_epilogue */
  debug_nothing_tree,		         /* begin_function */
  debug_nothing_int,		         /* end_function */
  debug_nothing_tree,		         /* register_main_translation_unit */
  debug_nothing_tree,		         /* function_decl */
  debug_nothing_tree,	         	 /* early_global_decl */
  debug_nothing_tree,	         	 /* late_global_decl */
  debug_nothing_tree_int,		 /* type_decl */
  debug_nothing_tree_tree_tree_bool_bool,/* imported_module_or_decl */
  debug_false_tree_charstarstar_uhwistar,/* die_ref_for_decl */
  debug_nothing_tree_charstar_uhwi,      /* register_external_die */
  debug_nothing_tree,		         /* deferred_inline_function */
  debug_nothing_tree,		         /* outlining_inline_function */
  debug_nothing_rtx_code_label,	         /* label */
  debug_nothing_int,		         /* handle_pch */
  debug_nothing_rtx_insn,	         /* var_location */
  debug_nothing_tree,	         	 /* inline_entry */
  debug_nothing_tree,			 /* size_function */
  debug_nothing_void,                    /* switch_text_section */
  debug_nothing_tree_tree,		 /* set_name */
  0,                                     /* start_end_main_source_file */
  TYPE_SYMTAB_IS_ADDRESS                 /* tree_type_symtab_field */
};

/* This file contains implementations of each debug hook that do
   nothing.  */

void
debug_nothing_void (void)
{
}

void
debug_nothing_tree (tree decl ATTRIBUTE_UNUSED)
{
}

void
debug_nothing_tree_tree (tree t1 ATTRIBUTE_UNUSED,
			 tree t2 ATTRIBUTE_UNUSED)
{
}

void
debug_nothing_tree_tree_tree_bool_bool (tree t1 ATTRIBUTE_UNUSED,
					tree t2 ATTRIBUTE_UNUSED,
					tree t3 ATTRIBUTE_UNUSED,
					bool b1 ATTRIBUTE_UNUSED,
					bool b2 ATTRIBUTE_UNUSED)
{
}

bool
debug_true_const_tree (const_tree block ATTRIBUTE_UNUSED)
{
  return true;
}

void
debug_nothing_rtx_insn (rtx_insn *insn ATTRIBUTE_UNUSED)
{
}

void
debug_nothing_rtx_code_label (rtx_code_label *label ATTRIBUTE_UNUSED)
{
}

void
debug_nothing_charstar (const char *main_filename ATTRIBUTE_UNUSED)
{
}

void
debug_nothing_int_charstar (unsigned int line ATTRIBUTE_UNUSED,
			    const char *text ATTRIBUTE_UNUSED)
{
}

void
debug_nothing_int_int_charstar (unsigned int line ATTRIBUTE_UNUSED,
				unsigned int column ATTRIBUTE_UNUSED,
				const char *text ATTRIBUTE_UNUSED)
{
}

void
debug_nothing_int_int_charstar_int_bool (unsigned int line ATTRIBUTE_UNUSED,
					 unsigned int column ATTRIBUTE_UNUSED,
					 const char *text ATTRIBUTE_UNUSED,
					 int discriminator ATTRIBUTE_UNUSED,
					 bool is_stmt ATTRIBUTE_UNUSED)
{
}

void
debug_nothing_int (unsigned int line ATTRIBUTE_UNUSED)
{
}

void
debug_nothing_int_int (unsigned int line ATTRIBUTE_UNUSED,
		       unsigned int n ATTRIBUTE_UNUSED)
{
}

void
debug_nothing_tree_int (tree decl ATTRIBUTE_UNUSED,
			int local ATTRIBUTE_UNUSED)
{
}

bool
debug_false_tree_charstarstar_uhwistar (tree, const char **,
					unsigned HOST_WIDE_INT *)
{
  return false;
}

void
debug_nothing_tree_charstar_uhwi (tree, const char *,
				  unsigned HOST_WIDE_INT)
{
}

/* Do-nothing debug hooks for GCC.
   Copyright (C) 2001, 2002 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "debug.h"

/* The do-nothing debug hooks.  */
struct gcc_debug_hooks do_nothing_debug_hooks =
{
  debug_nothing_charstar,
  debug_nothing_charstar,
  debug_nothing_int_charstar,
  debug_nothing_int_charstar,
  debug_nothing_int_charstar,
  debug_nothing_int,
  debug_nothing_int_int,	/* begin_block */
  debug_nothing_int_int,	/* end_block */
  debug_true_tree,		/* ignore_block */
  debug_nothing_int_charstar,	/* source_line */
  debug_nothing_int_charstar,	/* begin_prologue */
  debug_nothing_int,		/* end_prologue */
  debug_nothing_void,		/* end_epilogue */
  debug_nothing_tree,		/* begin_function */
  debug_nothing_int,		/* end_function */
  debug_nothing_tree,		/* function_decl */
  debug_nothing_tree,		/* global_decl */
  debug_nothing_tree,		/* deferred_inline_function */
  debug_nothing_tree,		/* outlining_inline_function */
  debug_nothing_rtx		/* label */
};

/* This file contains implementations of each debug hook that do
   nothing.  */

void
debug_nothing_void ()
{
}

void
debug_nothing_tree (decl)
     tree decl ATTRIBUTE_UNUSED;
{
}

bool
debug_true_tree (block)
     tree block ATTRIBUTE_UNUSED;
{
  return true;
}

void
debug_nothing_rtx (insn)
     rtx insn ATTRIBUTE_UNUSED;
{
}

void
debug_nothing_charstar (main_filename)
     const char *main_filename ATTRIBUTE_UNUSED;
{
}

void
debug_nothing_int_charstar (line, text)
     unsigned int line ATTRIBUTE_UNUSED;
     const char *text ATTRIBUTE_UNUSED;
{
}

void
debug_nothing_int (line)
     unsigned int line ATTRIBUTE_UNUSED;
{
}

void
debug_nothing_int_int (line, n)
     unsigned int line ATTRIBUTE_UNUSED;
     unsigned int n ATTRIBUTE_UNUSED;
{
}

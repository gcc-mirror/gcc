/* Handle #pragma, system V.4 style.  Supports #pragma weak and #pragma pack.
   Copyright (C) 1992 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <stdio.h>
#include "config.h"
#include "tree.h"
#include "function.h"
#include "defaults.h"

#ifdef HANDLE_SYSV_PRAGMA

/* Support #pragma weak by default if WEAK_ASM_OP and ASM_OUTPUT_DEF
   are defined.  */
#if !defined (HANDLE_PRAGMA_WEAK) && defined (WEAK_ASM_OP) && defined (ASM_OUTPUT_DEF)
#define HANDLE_PRAGMA_WEAK 1
#endif

/* See varasm.c for an identical definition.  */
enum pragma_state
{
  ps_start,
  ps_done,
  ps_bad,
  ps_weak,
  ps_name,
  ps_equals,
  ps_value,
  ps_pack,
  ps_left,
  ps_align,
  ps_right
};

/* When structure field packing is in effect, this variable is the
   number of bits to use as the maximum alignment.  When packing is not
   in effect, this is zero. */

extern int maximum_field_alignment;

/* File used for outputting assembler code.  */
extern FILE *asm_out_file;

/* Handle one token of a pragma directive.  TOKEN is the
   current token, and STRING is its printable form.  */

void
handle_pragma_token (string, token)
     char *string;
     tree token;
{
  static enum pragma_state state = ps_start, type;
  static char *name;
  static char *value;
  static int align;

  if (string == 0)
    {
      if (type == ps_pack)
	{
	  if (state == ps_right)
	    maximum_field_alignment = align * 8;
	  else
	    warning ("malformed `#pragma pack'");
	}
      else if (type == ps_weak)
	{
#ifdef HANDLE_PRAGMA_WEAK
	  if (HANDLE_PRAGMA_WEAK)
	    handle_pragma_weak (state, asm_out_file, name, value);

#endif /* HANDLE_PRAMA_WEAK */
	}

      type = state = ps_start;
      return;
    }

  switch (state)
    {
    case ps_start:
      if (token && TREE_CODE (token) == IDENTIFIER_NODE)
	{
	  if (strcmp (IDENTIFIER_POINTER (token), "pack") == 0)
	    type = state = ps_pack;
	  else if (strcmp (IDENTIFIER_POINTER (token), "weak") == 0)
	    type = state = ps_weak;
	  else
	    type = state = ps_done;
	}
      else
	type = state = ps_done;
      break;

    case ps_weak:
      if (token && TREE_CODE (token) == IDENTIFIER_NODE)
	{
	  name = IDENTIFIER_POINTER (token);
	  state = ps_name;
	}
      else
	state = ps_bad;
      break;

    case ps_name:
      state = (strcmp (string, "=") ? ps_bad : ps_equals);
      break;

    case ps_equals:
      if (token && TREE_CODE (token) == IDENTIFIER_NODE)
	{
	  value = IDENTIFIER_POINTER (token);
	  state = ps_value;
	}
      else
	state = ps_bad;
      break;

    case ps_value:
      state = ps_bad;
      break;

    case ps_pack:
      if (strcmp (string, "(") == 0)
	state = ps_left;
      else
	state = ps_bad;
      break;

    case ps_left:
      if (token && TREE_CODE (token) == INTEGER_CST
	  && TREE_INT_CST_HIGH (token) == 0)
	switch (TREE_INT_CST_LOW (token))
	  {
	  case 1:
	  case 2:
	  case 4:
	    align = TREE_INT_CST_LOW (token);
	    state = ps_align;
	    break;

	  default:
	    state = ps_bad;
	  }
      else if (! token && strcmp (string, ")") == 0)
	{
	  align = 0;
	  state = ps_right;
	}
      else
	state = ps_bad;
      break;

    case ps_align:
      if (strcmp (string, ")") == 0)
	state = ps_right;
      else
	state = ps_bad;
      break;

    case ps_right:
      state = ps_bad;
      break;

    case ps_bad:
    case ps_done:
      break;

    default:
      abort ();
    }
}
#endif /* HANDLE_SYSV_PRAGMA */

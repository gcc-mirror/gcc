/* Pragma related interfaces.
   Copyright (C) 1995 Free Software Foundation, Inc.

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

/* Support #pragma weak iff ASM_WEAKEN_LABEL and ASM_OUTPUT_DEF are
   defined.  */
#if defined (ASM_WEAKEN_LABEL) && defined (ASM_OUTPUT_DEF)
#define HANDLE_PRAGMA_WEAK SUPPORTS_WEAK
#endif

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

/* Output asm to handle ``#pragma weak'' */
extern void handle_pragma_weak PROTO((enum pragma_state, char *, char *));

/* Handle a C style pragma */
extern void handle_pragma_token PROTO((char *, tree));

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifdef HANDLE_SYSV_PRAGMA

/* Support #pragma weak by default if WEAK_ASM_OP and ASM_OUTPUT_DEF
   are defined.  */
#if !defined (HANDLE_PRAGMA_WEAK) && defined (WEAK_ASM_OP) && defined (ASM_OUTPUT_DEF)
#define HANDLE_PRAGMA_WEAK 1
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
#endif /* HANDLE_SYSV_PRAGMA */

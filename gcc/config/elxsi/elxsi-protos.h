/* Definitions of target machine for GNU compiler.  Elxsi version.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Mike Stump <mrs@cygnus.com> in 1988.  This is the first
   64 bit port of GNU CC.
   Based upon the VAX port.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Functions used in the md file. */

#ifdef RTX_CODE
extern const char *cmp_set PARAMS ((const char *, const char *, rtx));
extern const char *cmp_jmp PARAMS ((const char *, int, rtx));
extern void print_operand_address PARAMS ((FILE *, rtx));
#endif /* RTX_CODE */


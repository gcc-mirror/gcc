/* Definitions of target machine for GNU compiler.  AT&T we32000 version.
   Copyright (C) 2000
   Free Software Foundation, Inc.
   Contributed by John Wehle (john@feith1.uucp)

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

#ifdef RTX_CODE
extern void output_move_double PARAMS ((rtx *));
extern void output_push_double PARAMS ((rtx *));
#endif /* RTX_CODE */

/* Profiling support code for A/UX 
   Copyright (C) 1996 Free Software Foundation, Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* This routine is called at the beginning of functions compiled with -p
   or -pg.  The A/UX libraries call mcount%, but gas cannot generate 
   symbols with embedded percent signs.  Previous ports of GCC to A/UX 
   have done things like (1) assemble a stub routine with the native
   assembler, or (2) assemble a stub routine with gas and edit the object
   file.  This solution has the advantage that it can interoperate with
   the A/UX version and can be used in an eventual port of glibc to A/UX.  */

#ifndef __GNUC__
#error This file uses GNU C extensions
#endif

#include "tconfig.h"
#include <mon.h>

struct cnt *_countbase;

#ifdef FUNCTION_PROFILER_SYMBOL
void __mcount() __asm__(FUNCTION_PROFILER_SYMBOL);
#endif

void __mcount()
{
  register long **pfncnt __asm__("%a0");
  register long *fncnt = *pfncnt;

  if (!fncnt)
    {
      struct cnt *newcnt = _countbase++;
      newcnt->fnpc = (char *)__builtin_return_address(0);
      *pfncnt = fncnt = &newcnt->mcnt;
    }
  *fncnt += 1;
}

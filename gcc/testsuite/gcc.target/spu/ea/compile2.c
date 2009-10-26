/* Copyright (C) 2009 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with this file; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* Make sure __ea structure references work.  */

/* { dg-do compile } */

typedef unsigned long int uintptr_t;

struct tostruct
{
  uintptr_t selfpc;
  long count;
  unsigned short link;
};

/* froms are indexing tos */
static __ea unsigned short *froms;
static __ea struct tostruct *tos = 0;

void
foo (uintptr_t frompc, uintptr_t selfpc)
{
  __ea unsigned short *frompcindex;

  frompcindex = &froms[(frompc) / (4 * sizeof (*froms))];
  *frompcindex = tos[0].link;

  return;
}

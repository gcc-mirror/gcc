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

/* { dg-do run } */

extern void abort (void);
extern unsigned long long __ea_local_store;

__ea int *ppu;
int x, *spu = &x, *spu2;

int
main (int argc, char **argv)
{
  ppu = (__ea int *) spu;
  spu2 = (int *) ppu;

#ifdef __EA32__
  if ((int) ppu != (int) __ea_local_store + (int) spu)
#else
  if ((unsigned long long) ppu != __ea_local_store + (unsigned long long)(int) spu)
#endif

    abort ();

  if (spu != spu2)
    abort ();

  return 0;
}

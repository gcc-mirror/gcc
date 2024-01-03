/* Copyright (C) 2008-2024 Free Software Foundation, Inc. */
/* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. */

#include <stdio.h>
#include <stdlib.h>

typedef unsigned int INTEGER;
INTEGER *ip;

void fly (void)
{
  if (ip == NULL)
     throw (INTEGER) 1;
  if ((4 / (*ip)) == 4)
    printf("yes it worked\n");
}

/*
 *   a heavily reduced test case, to aid debugging the compiler..
 */

void tryFlying (void)
{
 again:
  try {
    fly();
#if 0
    goto fin;
#endif
  }
  catch (...) {
#if 1
    if (ip == NULL) {
#endif
      ip = (unsigned int *)malloc(sizeof(unsigned int));
      *ip = 1;
      goto again;
#if 1
    }
#endif
  }
 fin:;
}

main()
{
  ip = NULL;
  tryFlying();
  printf("all done\n");
}

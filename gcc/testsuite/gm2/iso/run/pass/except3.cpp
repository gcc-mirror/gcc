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

// a c++ example of Modula-2 exception handling

static int *ip = NULL;

void fly (void)
{
  printf("fly main body\n") ;
  if (ip == NULL)
    throw;
  if (*ip == 0)
    throw;
  if (4 / (*ip) == 4)
    printf("yes it worked\n");
  else
    printf("no it failed\n");
}

/*
 *   a CPP version of the Modula-2 example given in the ISO standard.
 *   This is a hand translation of the equivalent except2.mod file in this
 *   directory which is written to prove that the underlying CPP
 *   runtime system will support ISO Modula-2 exceptions and to reinforce
 *   my understanding of how the GCC trees are constructed and what
 *   state is held where..
 */

void tryFlying (void)
{
 again_tryFlying:
  printf("tryFlying main body\n");  
  try {
    fly() ;
  }
  catch (...) {
    printf("inside tryFlying exception routine\n") ;
    if ((ip != NULL) && ((*ip) == 0)) {
      *ip = 1;
      // retry
      goto again_tryFlying;
    }
    printf("did't handle exception here so we will call the next exception routine\n") ;
    throw;  // unhandled therefore call previous exception handler
  }
}

void keepFlying (void)
{
 again_keepFlying:
  printf("keepFlying main body\n") ;
  try {
    tryFlying();
  }
  catch (...) {
    printf("inside keepFlying exception routine\n");
    if (ip == NULL) {
      ip = (int *)malloc(sizeof(int));
      *ip = 0;
      goto again_keepFlying;
    }
    throw;  // unhandled therefore call previous exception handler
  }
}

main ()
{
  keepFlying();
  printf("all done\n");
}

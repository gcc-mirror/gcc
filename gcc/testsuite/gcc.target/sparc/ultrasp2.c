/* Copyright (C) 1999 Free Software Foundation
   by Alexandre Oliva <oliva@lsd.ic.unicamp.br>
   Simplified from libg++/src/Fix16.cc */

/* { dg-do compile } */
/* { dg-options "-O0" } */

short foo() {
  short i = (short)(1<<15);
  return i;
}

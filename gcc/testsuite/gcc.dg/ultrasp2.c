/* Copyright (C) 1999 Free Software Foundation
   by Alexandre Oliva <oliva@lsd.ic.unicamp.br>
   Simplified from libg++/src/Fix16.cc */

/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-options "-O0 -m64" { target sparc64-*-* } } */
/* { dg-options "-O0 -m64" { target sparcv9-*-* } } */
/* { dg-options "" { target sparc-*-solaris2.[0-6] } } */
/* { dg-options "" { target sparc-*-solaris2.[0-6].* } } */

short foo() {
  short i = (short)(1<<15);
  return i;
}

/* Copyright (C) 2000  Free Software Foundation.

   by Alexandre Oliva  <oliva@lsd.ic.unicamp.br>  */

/* { dg-do run } */
/* { dg-options "-O3" } */

extern void abort (void);

void foo () {} /* unused, but essential to trigger the bug */

int main () {
  int i;
  /* use asms to prevent optimizations */
  /* i = -1; */ asm ("" : "=r" (i) : "0" (-1));
  /* i =  1; */ asm ("" : "=r" (i) : "0" (i ? 1 : 2));
  if (i != 1)
    abort();
  return 0;
}

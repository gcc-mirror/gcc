/* { dg-do compile } */
/* { dg-options "-O2" } */

void ext(int x, int y);

void foo(int x, int y) { ext(y,x); }

/* { dg-final { scan-assembler "swpw r3,r2" } } */

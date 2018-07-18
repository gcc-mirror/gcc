/* { dg-do compile } */
/* { dg-options "-O2 -floop-nest-optimize -Wno-aggressive-loop-optimizations" } */

int a;
int b[6];
int c ()
{
  int d;
  for (; d; d++)
    b[d] = 0;
  for (; d < 8; d++)
    a += b[d];
}

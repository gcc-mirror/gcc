/* { dg-do compile } */
/* { dg-options "-Os -fbranch-count-reg" } */

/* This caused an assertion within arc_loop_hazard.  */

unsigned a, b;

long fn1()
{
  long c = 1, d = 0;
  while (a && c && b)
    c <<= 1;
  while (c)
    d |= c;
  return d;
}

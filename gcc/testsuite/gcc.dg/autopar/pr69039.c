/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=2" } */

unsigned int b;

unsigned int
fn1 (unsigned int d)
{
  int i;

  for (i = 0; i < 1000; i++)
    b |= d;

  return b;
}

/* { dg-do compile } */
/* { dg-additional-options "-O3" } */
/* { dg-require-effective-target vect_unpack } */

void foo(unsigned *p1, unsigned short *p2)
{
  int n;
  for (n = 0; n < 320; n++)
    p1[n] = p2[n * 2];
}

/* { dg-final { scan-tree-dump "BB vectorization with gaps at the end of a load is not supported" "slp1" } } */

/* { dg-do compile } */
/* { dg-require-effective-target pthread } */
/* { dg-options "-O2 -ftree-parallelize-loops=2 -fno-tree-loop-im" } */

extern int *b, *e[8], d;

void
foo (void)
{
  int i;
  for (i = 0; i < 8; ++i)
    while (--d)
      e[i] = 0;
}

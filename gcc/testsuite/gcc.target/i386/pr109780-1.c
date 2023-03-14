/* { dg-do compile } */
/* { dg-options "-O3 -march=skylake" } */

char perm[64];

void
__attribute__((noipa))
foo (int n)
{
  for (int i = 0; i < n; ++i)
    perm[i] = i;
}

/* { dg-final { scan-assembler-not "and\[lq\]?\[^\\n\]*-32,\[^\\n\]*sp" } } */

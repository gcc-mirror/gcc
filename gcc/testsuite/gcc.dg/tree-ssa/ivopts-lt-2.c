/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ivopts" } */
/* { dg-skip-if "PR68644" { hppa*-*-* } { "*" } { "" } } */

void
f1 (int *p, unsigned int i)
{
  p += i;
  do
    {
      *p = 0;
      p += 1;
      i++;
    }
  while (i < 100);
}

/* { dg-final { scan-tree-dump-times "PHI" 1 "ivopts" } } */
/* { dg-final { scan-tree-dump-times "PHI <p_" 1 "ivopts"} } */
/* { dg-final { scan-tree-dump-times "p_\[0-9\]* <" 1 "ivopts" } } */

/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* For powerpc, disable doloop IV cand generation in IVOPTs to avoid unexpected
   division operation for its base setup.  */
/* { dg-additional-options "-fno-branch-count-reg" { target { powerpc*-*-* } } } */

int foo (int n)
{
  while (n >= 45)
    n -= 45;

  return n;
}

int bar (int n)
{
  while (n >= 64)
    n -= 64;

  return n;
}

int bla (int n)
{
  int i = 0;

  while (n >= 45)
    {
      i++;
      n -= 45;
    }

  return i;
}

int baz (int n)
{
  int i = 0;

  while (n >= 64)
    {
      i++;
      n -= 64;
    }

  return i;
}

/* The loops computing division/modulo by 64 should be eliminated */
/* { dg-final { scan-tree-dump-times "if" 6 "optimized" } } */

/* There should be no division/modulo in the final dump (division and modulo
   by 64 are done using bit operations).  */
/* { dg-final { scan-tree-dump-times " / " 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times " % " 0 "optimized" } } */


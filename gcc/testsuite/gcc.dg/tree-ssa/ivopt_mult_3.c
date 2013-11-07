/* { dg-do compile { target {{ i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-O2 -m64 -fdump-tree-ivopts-details" } */

/* iv p2 can be eliminated.  */
long foo(long* p, long* p2, int N1, int N2)
{
  unsigned long  i = 0;
  long* p_limit2 = p2 + N2;
  long s = 0;
  while (i < N1)
    {
      p2++;
      i++;
      if (p2 > p_limit2)
        break;
      s += p[i];
    }
  return s;
}

/* { dg-final { scan-tree-dump-times "Replacing exit test: if \\(.*p2.*\\)" 1 "ivopts"} } */
/* { dg-final { cleanup-tree-dump "ivopts" } } */


/* { dg-do compile { target {{ i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-O2 -m64 -fdump-tree-optimized" } */

/* iv i's step 16 so its period is smaller than the max iterations
 * i.e. replacing if (p2 > p_limit2) with testing of i may result in
 * overflow.  */
long foo(long* p, long* p2, int N1, int N2)
{
  unsigned long  i = 0;
  long* p_limit2 = p2 + N2;
  long s = 0;
  while (i < N1)
    {
      p2++;
      i += 16;
      if (p2 > p_limit2)
        break;
     s += p[i];
  }
  return s;
}

/* { dg-final { scan-tree-dump "if \\(.*p_limit2.*\\)" "optimized"} } */

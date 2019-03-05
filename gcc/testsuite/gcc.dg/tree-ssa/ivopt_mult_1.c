/* { dg-do compile { target {{ i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-O2 -m64 -fdump-tree-ivopts-details" } */

/* The test 'if (p2 > p_limit2)' can be replaced, so iv p2 can be
 * eliminated.  */
long foo(long* p, long* p2, int N1, int N2)
{
  int i = 0;
  long* p_limit = p + N1;
  long* p_limit2 = p2 + N2;
  long s = 0;
  while (p  <= p_limit)
    {
      p++;
      p2++;
      if (p2 > p_limit2)
        break;
      s += (*p);
    }
  return s;
}

/* { dg-final { scan-tree-dump-times "Replacing" 1 "ivopts" } } */

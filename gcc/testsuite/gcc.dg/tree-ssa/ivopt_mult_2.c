/* { dg-do compile { target {{ i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-O2 -m64 -fdump-tree-ivopts-details" } */

/* Exit tests 'i < N1' and 'p2 > p_limit2' can be replaced, so
 * two ivs i and p2 can be eliminate.  */
long foo(long* p, long* p2, int N1, int N2)
{
  int i = 0;
  long* p_limit2 = p2 + N2;
  long s = 0;
  while (i < N1)
    {
       p++;
       p2++;
       i++;
       if (p2 > p_limit2)
         break;
       s += (*p);
    }

  return s;
}

/* { dg-final { scan-tree-dump-times "Replacing" 2 "ivopts" { xfail *-*-* } } } */

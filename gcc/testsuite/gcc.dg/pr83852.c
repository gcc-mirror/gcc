/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-std=gnu99 -fpermissive -O2 -fselective-scheduling -fno-if-conversion -fno-tree-dse -w" } */
long long int uo;
unsigned int vt;

void
r5 (long long int h8, long long int pu)
{
  short int wj;
  long long int *mh = h8;

  for (wj = 0; wj < 3; ++wj)
    {
      int oq;
      long long int ns, xf;

      h8 += 2;
      oq = !!h8 && !!wj;
      ++uo;
      vt ^= oq + uo;
      ns = !!uo && !!vt;
      xf = (h8 != 0) ? mh : 1;
      pu += ns < xf;
    }

  for (pu = 0; pu < 1; ++pu)
    {
      int *sc;

      sc = (int *)&pu;
      *sc = 0;
    }
}

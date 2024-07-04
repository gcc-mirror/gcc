/* { dg-options "-O3 -fgraphite-identity -fsave-optimization-record" } */

int long_c2i_ltmp;
int *long_c2i_cont;

void
long_c2i (long utmp, int i)
{
  int neg = 1;
  switch (long_c2i_cont[0])
    case 0:
    neg = 0;
  for (; i; i++)
    if (neg)
      utmp |= long_c2i_cont[i] ^ 5;
    else
      utmp |= long_c2i_cont[i];
  long_c2i_ltmp = utmp;
}

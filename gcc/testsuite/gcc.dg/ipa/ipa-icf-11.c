/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf-optimized"  } */

__attribute__ ((noinline))
int fce(int a, int b)
{
  return a + b;
}

__attribute__ ((noinline))
int f0(int a)
{
  return fce(a, 5) + fce(a, 7);
}

__attribute__ ((noinline))
int f1(int a)
{
  return fce(a, 5) + fce(a, 7);
}

int main(int argc, char **argv)
{
  return f0(argc) * f1(argc);
}

/* { dg-final { scan-ipa-dump "Semantic equality hit:f0/\[0-9+\]+->f1/\[0-9+\]+" "icf"  } } */
/* { dg-final { scan-ipa-dump "Equal symbols: 1" "icf"  } } */

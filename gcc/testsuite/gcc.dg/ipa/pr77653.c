/* { dg-require-alias "" } */
/* { dg-options "-O2 -fcommon -fdump-ipa-icf-details"  } */

int a, b, c, d, e, h, i, j, k, l;
const int f;
static int g;

void fn1 (int p1)
{
  k = d ? c % k : a * b;
  h &= j ^ i ^ 1;
}

int main ()
{
  const int *m = &f, **n = &m;
  l && (*n = &e);
  if (m != &f)
    __builtin_abort ();
  fn1 (g);
  return 0;
}

/* { dg-final { scan-ipa-dump "Equal symbols: 1" "icf"  } } */
/* { dg-final { scan-ipa-dump "Not unifying; alias cannot be created; target is discardable" "icf"  } } */

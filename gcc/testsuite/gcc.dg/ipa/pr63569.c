/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf-details"  } */

static int f(int t, int *a) __attribute__((noinline));

static int g(int t, volatile int *a) __attribute__((noinline));
static int g(int t, volatile int *a)
{
  int i;
  int tt = 0;
  for(i=0;i<t;i++)
    tt += *a;
  return tt;
}
static int f(int t, int *a)
{
  int i;
  int tt = 0;
  for(i=0;i<t;i++)
    tt += *a;
  return tt;
}


int h(int t, int *a)
{
  return f(t, a) + g(t, a);
}

/* { dg-final { scan-ipa-dump "Equal symbols: 0" "icf"  } } */

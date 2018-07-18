/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-pure-const-details" } */

static void *foo(__SIZE_TYPE__, int) __attribute__((noinline, no_icf, used));

__attribute__((noinline, used, no_icf))
static void *bar(__SIZE_TYPE__ n, int m)
{
  return foo (n, m);
}

static void *foo(__SIZE_TYPE__ n, int m)
{
  void *p;
  if (m > 0)
    p = bar (n, --m);
  else
    p = __builtin_malloc (n);

  return p;
}

/* { dg-final { scan-ipa-dump "Function foo found to be malloc" "pure-const" } } */
/* { dg-final { scan-ipa-dump "Function bar found to be malloc" "pure-const" } } */

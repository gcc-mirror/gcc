/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-pure-const-details -fdelete-null-pointer-checks" } */
/* { dg-skip-if "" keeps_null_pointer_checks } */

__attribute__((noinline, used, no_icf))
static void *foo (__SIZE_TYPE__ n)
{
  return __builtin_malloc (n * 10);
}

__attribute__((noinline, used, no_icf))
static void *bar(__SIZE_TYPE__ n, int cond)
{
  void *p;
  if (cond)
    p = foo (n);
  else
    p = __builtin_malloc (n);

  return p;
}

/* { dg-final { scan-ipa-dump "Function foo/\[0-9+\]+ found to be malloc" "pure-const" } } */
/* { dg-final { scan-ipa-dump "Function bar/\[0-9+\]+ found to be malloc" "pure-const" } } */

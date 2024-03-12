/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-pure-const-details -fdelete-null-pointer-checks" } */
/* { dg-skip-if "" keeps_null_pointer_checks } */

__attribute__((noinline, no_icf, used))
static void *f(__SIZE_TYPE__ n)
{
  void *p = __builtin_malloc (n);
  if (p == 0)
    __builtin_abort ();
  return p;
}

__attribute__((noinline, no_icf, used))
static void *bar(__SIZE_TYPE__ n)
{
  void *p = f (n);
  return p;
}

/* { dg-final { scan-ipa-dump "Function f/\[0-9+\]+ found to be malloc" "pure-const" } } */
/* { dg-final { scan-ipa-dump "Function bar/\[0-9+\]+ found to be malloc" "pure-const" } } */

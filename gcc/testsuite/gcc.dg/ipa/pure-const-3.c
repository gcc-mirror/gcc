/* { dg-do run } */
/* { dg-require-alias "" }  */
/* { dg-options "-O2 -fdump-tree-local-pure-const1" } */

__attribute__ ((weak))
__attribute__ ((noinline))
int a(int v)
{
  return v;
}
__attribute__ ((noinline))
static int b(int v) __attribute__ ((alias("a")));
int
main()
{
  int c = a(1)==a(1);
  int d = b(1)==b(1);
  if (__builtin_constant_p (c))
    __builtin_abort ();
  if (!__builtin_constant_p (d))
    __builtin_abort ();
  return 0;
}
/* { dg-final { scan-tree-dump "found to be const" "local-pure-const1"} } */

/* PR rtl-optimization/82192 */

unsigned long long int a = 0x95dd3d896f7422e2ULL;
struct S { unsigned int m : 13; } b;

__attribute__((noinline, noclone)) void
foo (void)
{
  b.m = ((unsigned) a) >> (0x644eee9667723bf7LL
			   | a & ~0xdee27af8U) - 0x644eee9667763bd8LL;
}

int
main ()
{
  if (__INT_MAX__ != 0x7fffffffULL)
    return 0;
  foo ();
  if (b.m != 0)
    __builtin_abort ();
  return 0;
}

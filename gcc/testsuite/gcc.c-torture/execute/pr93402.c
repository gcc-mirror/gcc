/* PR rtl-optimization/93402 */

struct S { unsigned int a; unsigned long long b; };

__attribute__((noipa)) struct S
foo (unsigned long long x)
{
  struct S ret;
  ret.a = 0;
  ret.b = x * 11111111111ULL + 111111111111ULL;
  return ret;
}

int
main ()
{
  struct S a = foo (1);
  if (a.a != 0 || a.b != 122222222222ULL)
    __builtin_abort ();
  return 0;
}

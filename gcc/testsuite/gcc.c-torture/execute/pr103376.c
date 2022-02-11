/* PR tree-optimization/103376 */

long long a = 0x123456789abcdef0LL, f;
int b, c, *d;

__attribute__((noipa)) void
foo (int x)
{
  asm volatile ("" : : "r" (x));
}

int
main ()
{
  long long e;
  e = a;
  if (b)
    {
      foo (c);
      d = (int *) 0;
      while (*d)
	;
    }
  f = a ^ e;
  asm volatile ("" : "+m" (f));
  if (f != 0)
    __builtin_abort ();
  return 0;
}

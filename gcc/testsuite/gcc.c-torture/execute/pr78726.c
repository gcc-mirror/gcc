/* PR tree-optimization/78726 */

unsigned char b = 36, c = 173;
unsigned int d;

__attribute__((noinline, noclone)) void
foo (void)
{
  unsigned a = ~b;
  d = a * c * c + 1023094746U * a;
}

int
main ()
{
  if (__SIZEOF_INT__ != 4 || __CHAR_BIT__ != 8)
    return 0;
  asm volatile ("" : : "g" (&b), "g" (&c) : "memory");
  foo ();
  if (d != 799092689U)
    __builtin_abort ();
  return 0;
}

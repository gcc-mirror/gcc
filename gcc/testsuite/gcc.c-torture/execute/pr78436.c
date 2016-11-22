/* PR tree-optimization/78436 */

struct S
{
  long int a : 24;
  signed char b : 8;
} s;

__attribute__((noinline, noclone)) void
foo ()
{
  s.b = 0;
  s.a = -1193165L;
}

int
main ()
{
  foo ();
  if (s.b != 0)
    __builtin_abort ();
  return 0;
}

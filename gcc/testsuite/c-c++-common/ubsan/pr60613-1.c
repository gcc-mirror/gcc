/* PR sanitizer/60613 */
/* { dg-do run } */
/* { dg-options "-fsanitize=undefined" } */

long long y;

__attribute__((noinline, noclone)) long long
foo (long long x)
{
  asm ("");
  if (x >= 0 || x < -2040)
    return 23;
  x += 2040;
  return x - y;
}

__attribute__((noinline, noclone)) long long
bar (long long x)
{
  asm ("");
  return 8LL - x;
}

int
main ()
{
  y = 1;
  if (foo (8 - 2040) != 8 - 1)
    __builtin_abort ();
  if (bar (1) != 8 - 1)
    __builtin_abort ();
  return 0;
}

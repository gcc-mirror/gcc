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
  y = -__LONG_LONG_MAX__ + 6;
  if (foo (8 - 2040) != -__LONG_LONG_MAX__)
    __builtin_abort ();
  if (bar (-__LONG_LONG_MAX__ + 5) != -__LONG_LONG_MAX__ + 1)
    __builtin_abort ();
  return 0;
}

/* { dg-output "signed integer overflow: 8 \\- -9223372036854775801 cannot be represented in type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: 8 \\- -9223372036854775802 cannot be represented in type 'long long int'" } */

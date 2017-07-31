/* PR sanitizer/81604 */
/* { dg-do run } */
/* { dg-options "-fsanitize=bounds,signed-integer-overflow" } */

long a[10];

__attribute__((noinline, noclone)) long *
foo (int i)
{
  return &a[i];
}

__attribute__((noinline, noclone)) long
bar (long x, long y)
{
  return x * y;
}

int
main ()
{
  volatile int i = -1;
  volatile long l = __LONG_MAX__;
  long *volatile p;
  p = foo (i);
  l = bar (l, l);
  return 0;
}

/* { dg-output "index -1 out of bounds for type 'long int \\\[10\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: \[0-9]+ \\* \[0-9]+ cannot be represented in type 'long int'" } */

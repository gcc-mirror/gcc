/* PR sanitizer/71498 */
/* { dg-do run } */
/* { dg-options "-fsanitize=bounds -Wno-array-bounds" } */

struct S { int a[100]; int b, c; } s;

__attribute__((noinline, noclone)) int
foo (int x)
{
  return s.a[x];
}

__attribute__((noinline, noclone)) int
bar (int x)
{
  static int *d = &s.a[99];
  asm volatile ("" : : "r" (&d));
  return s.a[x];
}

int
main ()
{
  volatile int a = 0;
  a += foo (100);
  a += bar (100);
  return 0;
}

/* { dg-output "index 100 out of bounds for type 'int \\\[100\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 100 out of bounds for type 'int \\\[100\\\]'\[^\n\r]*(\n|\r\n|\r)" } */

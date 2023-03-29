/* PR target/78796 */
/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-mcmodel=large -fno-pie -no-pie" { target aarch64-*-* } } */
/* { dg-require-effective-target tls_runtime } */
/* { dg-add-options tls } */

struct S { int a, b, c, d, e; };
struct S t;
__thread struct S s;

__attribute__((used, noinline, noclone)) void
foo (int *x, int *y)
{
  asm volatile ("" : : "g" (x), "g" (y) : "memory");
  if (*x != 1 || *y != 2)
    __builtin_abort ();
}

__attribute__((used, noinline, noclone)) void
bar (void)
{
  foo (&t.c, &s.c);
}

int
main ()
{
  t.c = 1;
  s.c = 2;
  bar ();
  return 0;
}

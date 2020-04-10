/* PR rtl-optimization/94516 */
/* { dg-do run } */
/* { dg-additional-options "-fpie" { target pie } } */

struct S { unsigned char *a; unsigned int b; };
typedef int V __attribute__((vector_size (sizeof (int) * 4)));

__attribute__((noipa)) void
foo (const char *a, const char *b, const char *c, const struct S *d, int e, int f, int g, int h, int i)
{
  V v = { 1, 2, 3, 4 };
  asm volatile ("" : : "g" (&v) : "memory");
  v += (V) { 5, 6, 7, 8 };
  asm volatile ("" : : "g" (&v) : "memory");
}

__attribute__((noipa)) void
bar (void)
{
  const struct S s = { "foobarbaz", 9 };
  foo ("foo", (const char *) 0, "corge", &s, 0, 1, 0, -12, -31);
  foo ("bar", "quux", "qux", &s, 0, 0, 9, 0, 0);
  foo ("baz", (const char *) 0, "qux", &s, 1, 0, 0, -12, -32);
}

int
main ()
{
  bar ();
  return 0;
}

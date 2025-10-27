/* PR ipa/121023 */
/* { dg-do compile { target musttail } } */
/* { dg-options "-O2" } */

struct S { int a, b; };

[[gnu::noipa]] int
foo (struct S x, int y, int z)
{
  return x.a + y + z;
}

[[gnu::noinline]] static int
bar (struct S x, int y, int z)
{
  [[gnu::musttail]] return foo ((struct S) { x.a, 0 }, y, 1);
}

int
baz (int x)
{
  return bar ((struct S) { 1, 2 }, x, 2) + bar ((struct S) { 2, 3 }, x + 1, 2);
}

/* PR tree-optimization/127687 */
/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug -fdump-tree-forwprop1-details" } */
/* { dg-additional-options "-msse2" { target i?86-*-* x86_64-*-* } } */

typedef int V __attribute__((vector_size (4 * sizeof (int))));

[[gnu::noipa]] void
foo (V *x, V *y)
{
  V a = *x;
  V b = *y;
  V c = __builtin_shufflevector (a, a, 0, 2, 0, 2);
  V d = __builtin_shufflevector (a, a, 1, 3, 1, 3);
  V e = __builtin_shufflevector (b, b, 0, 2, 0, 2);
  V f = __builtin_shufflevector (b, b, 1, 3, 1, 3);
  V g = __builtin_shufflevector (c + d, c - d, 0, 4, 1, 5);
  V h = __builtin_shufflevector (e + f, e - f, 0, 4, 1, 5);
  *x = g;
  *y = h;
}

/* { dg-final { scan-tree-dump "Vec perm simplify sequences have been blended" "forwprop1" { target { aarch64*-*-* i?86-*-* x86_64-*-* } } } } */

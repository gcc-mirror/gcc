/* PR c/64918 */
/* { dg-do compile } */
/* { dg-options "-Wno-override-init-side-effects" } */

struct S { int m, n; };
union U { short s; long int l; };

void
foo (int i)
{
  int a[] = {
    [0] = ++i,
    [1] = i,
    [0] = 42
  };
  struct S s = {
    .n = ++i,
    .m = i,
    .n = i
  };
  union U u = {
    .s = i--,
    .l = 42
  };
}

/* PR c/64918 */
/* { dg-do compile } */
/* { dg-options "" } */

struct S { int m, n; };
union U { short s; long int l; };

void
foo (int i)
{
  int a[] = {
    [0] = ++i,
    [1] = i,
    [0] = 42	/* { dg-warning "initialized field with side-effects overwritten" } */
  };
  struct S s = {
    .n = ++i,
    .m = i,
    .n = i	/* { dg-warning "initialized field with side-effects overwritten" } */
  };
  union U u = {
    .s = i--,
    .l = 42	/* { dg-warning "initialized field with side-effects overwritten" } */
  };
}

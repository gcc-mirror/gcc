/* PR c/102245 */
/* { dg-options "-Wint-in-bool-context" } */
/* { dg-do compile } */

_Bool test1(_Bool x)
{
  return !(x << 0);  /* { dg-warning "boolean context" } */
}

_Bool test2(_Bool x)
{
  return !(x << 1);  /* { dg-warning "boolean context" } */
}

_Bool test3(_Bool x, int y)
{
  return !(x << y);  /* { dg-warning "boolean context" } */
}

_Bool test4(int x, int y)
{
  return !(x << y);  /* { dg-warning "boolean context" } */
}

_Bool test5(int x, int y)
{
  return !((x << y) << 0);  /* { dg-warning "boolean context" } */
}

int test6(_Bool x)
{
  int v = 0;
  return (v & ~1L) | (1L & (x << 0));  /* { dg-bogus "boolean context" } */
}


/* PR c/61240 */
/* { dg-do compile } */

typedef __PTRDIFF_TYPE__ ptrdiff_t;

ptrdiff_t
foo (ptrdiff_t a[4])
{
  int i[4];
  int *p = i + 2;
  static ptrdiff_t b = p - (p - 1);	/* { dg-bogus "integer overflow in expression" } */
  static ptrdiff_t c = (p - 1) - p;	/* { dg-bogus "integer overflow in expression" } */
  static ptrdiff_t d = (p - 2) - (p - 1);/* { dg-bogus "integer overflow in expression" } */
  static ptrdiff_t e = (p - 1) - (p - 2);/* { dg-bogus "integer overflow in expression" } */
  a[0] = p - (p - 1);			/* { dg-bogus "integer overflow in expression" } */
  a[1] = (p - 1) - p;			/* { dg-bogus "integer overflow in expression" } */
  a[2] = (p - 2) - (p - 1);		/* { dg-bogus "integer overflow in expression" } */
  a[3] = (p - 1) - (p - 2);		/* { dg-bogus "integer overflow in expression" } */
  return b + c + d + e;
}

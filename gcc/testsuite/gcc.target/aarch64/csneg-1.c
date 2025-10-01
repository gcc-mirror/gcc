/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

int
test_csneg32_condasn1(int w0,
		      int w1,
		      int w2,
		      int w3) {
  int w4;

  w4 = (w0 == w1) ? -w3 : w2;
  return w4;
}
/*
** test_csneg32_condasn1:
**	cmp\tw0, w1
**	csneg\tw0, w2, w3, ne
**	ret
*/

int
test_csneg32_condasn2(int w0,
		      int w1,
		      int w2,
		      int w3) {
  int w4;

  w4 = (w0 == w1) ? w3 : -w2;
  return w4;
}
/*
** test_csneg32_condasn2:
**	cmp\tw0, w1
**	csneg\tw0, w3, w2, eq
**	ret
*/

long long
test_csneg64_condasn1(long long x0,
		      long long x1,
		      long long x2,
		      long long x3) {
  long long x4;

  x4 = (x0 == x1) ? -x3 : x2;
  return x4;
}
/*
** test_csneg64_condasn1:
**	cmp\tx0, x1
**	csneg\tx0, x2, x3, ne
**	ret
*/

long long
test_csneg64_condasn2(long long x0,
		      long long x1,
		      long long x2,
		      long long x3) {
  long long x4;

  x4 = (x0 == x1) ? x3 : -x2;
  return x4;
}
/*
** test_csneg64_condasn2:
**	cmp\tx0, x1
**	csneg\tx0, x3, x2, eq
**	ret
*/

int test_csneg_cmp(int x)
{
  if (x > 3)
    x = -x;
  return x;
}
/*
** test_csneg_cmp:
**	cmp\tw0, 3
**	csneg\tw0, w0, w0, le
**	ret
*/

unsigned long long
test_csneg_uxtw (unsigned int a,
		 unsigned int b,
		 unsigned int c)
{
  unsigned int val;
  val = a ? b: -c;
  return val;
}
/*
** test_csneg_uxtw:
**	cmp\tw0, 0
**	csneg\tw0, w1, w2, ne
**	ret
*/

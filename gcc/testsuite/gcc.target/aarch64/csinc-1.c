/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

unsigned int
test_csinc32_ifcvt(unsigned int w0,
		   unsigned int w1,
		   unsigned int w2) {
  if (w0 == w1)
    ++ w2;

  return w2;
}
/*
** test_csinc32_ifcvt:
**	cmp\tw0, w1
**	cinc\tw0, w2, eq
**	ret
*/

unsigned int
test_csinc32_condasn1(unsigned int w0,
		      unsigned int w1,
		      unsigned int w2,
		      unsigned int w3) {
  unsigned int w4;

  w4 = (w0 == w1) ? (w3 + 1) : w2;
  return w4;
}
/*
** test_csinc32_condasn1:
**	cmp\tw0, w1
**	csinc\tw0, w2, w3, ne
**	ret
*/

unsigned int
test_csinc32_condasn2(unsigned int w0,
		      unsigned int w1,
		      unsigned int w2,
		      unsigned int w3) {
  unsigned int w4;

  w4 = (w0 == w1) ? w2 : (w3 + 1);
  return w4;
}
/*
** test_csinc32_condasn2:
**	cmp\tw0, w1
**	csinc\tw0, w2, w3, eq
**	ret
*/

unsigned long long
test_csinc64_ifcvt(unsigned long long x0,
		   unsigned long long x1,
		   unsigned long long x2) {
  if (x0 == x1)
    ++ x2;

  return x2;
}
/*
** test_csinc64_ifcvt:
**	cmp\tx0, x1
**	cinc\tx0, x2, eq
**	ret
*/

unsigned long long
test_csinc64_condasn1(unsigned long long x0,
		      unsigned long long x1,
		      unsigned long long x2,
		      unsigned long long x3) {
  unsigned long long x4;

  x4 = (x0 == x1) ? (x3 + 1) : x2;
  return x4;
}
/*
** test_csinc64_condasn1:
**	cmp\tx0, x1
**	csinc\tx0, x2, x3, ne
**	ret
*/

unsigned long long
test_csinc64_condasn2(unsigned long long x0,
		      unsigned long long x1,
		      unsigned long long x2,
		      unsigned long long x3) {
  unsigned long long x4;

  x4 = (x0 == x1) ? x2 : (x3 + 1);
  return x4;
}
/*
** test_csinc64_condasn2:
**	cmp\tx0, x1
**	csinc\tx0, x2, x3, eq
**	ret
*/

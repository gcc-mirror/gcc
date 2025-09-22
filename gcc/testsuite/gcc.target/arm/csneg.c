/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8_1m_main_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_arch_v8_1m_main } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** test_csneg32_condasn1:
**	cmp\tr0, r1
**	csneg\tr0, r3, r2, ne
**	bx\tlr
*/
int
test_csneg32_condasn1(int w0, int w1, int w2, int w3)
{
  int w4;
  w4 = (w0 == w1) ? -w2 : w3;
  return w4;
}

/*
** test_csneg32_condasn2:
**	cmp\tr0, r1
**	csneg\tr0, r3, r2, eq
**	bx\tlr
*/
int
test_csneg32_condasn2(int w0, int w1, int w2, int w3)
{
  int w4;
  w4 = (w0 == w1) ? w3 : -w2;
  return w4;
}

/*
** test_csneg_uxtw: { target arm_little_endian }
**	cmp\tr0, #0
**	csneg\tr0, r1, r2, ne
**	movs\tr1, #0
**	bx\tlr
*/
/*
** test_csneg_uxtw: { target { ! arm_little_endian } }
**	cmp\tr0, #0
**	csneg\tr1, r1, r2, ne
**	movs\tr0, #0
**	bx\tlr
*/
unsigned long long
test_csneg_uxtw (unsigned int a, unsigned int b, unsigned int c)
{
  unsigned int val;
  val = a ? b : -c;
  return val;
}

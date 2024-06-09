/* PR c/102989 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#if __BITINT_MAXWIDTH__ >= 257
void
foo (_BitInt(135) *p, _BitInt(193) *q, _BitInt(257) *r)
{
  r[0] = (((p[0] + p[1] + p[2]) + q[0] + (p[3] + p[4] + p[5])) + q[1]) + r[1] + (((p[6] + p[7] + p[8]) + q[2] + (p[9] + p[10] + p[11])) + q[3]) + r[2];
}
#else
void
foo (void)
{
}
#endif

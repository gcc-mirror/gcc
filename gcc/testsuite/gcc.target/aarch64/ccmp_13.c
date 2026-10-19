/* { dg-do compile } */
/* { dg-options "-O2 -fgimple" } */
/* PR middle-end/109832 */

/* A non-binary boolean definition is not a conditional-compare chain.  */

_Bool __GIMPLE (ssa, startwith ("expand"))
bool_not_ior (_Bool a, _Bool b)
{
  _Bool _1;
  _Bool _2;

  __BB(2):
  _1 = ~a_3(D);
  _2 = _1 | b_4(D);
  return _2;
}

/* { dg-final { scan-assembler-not {\tccmp\t} } } */

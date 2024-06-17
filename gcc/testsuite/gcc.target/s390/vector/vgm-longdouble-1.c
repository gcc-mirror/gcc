/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -march=z14 -mzarch -fdump-tree-optimized" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** test_longdouble_via_vgmb:
**     vgmb	(%v[0-9]+),4,6
**     vst	\1,0\(%r2\),3
**     br	%r14
*/

long double
test_longdouble_via_vgmb (void)
{
  return 2.263171865473961260249112278523378513150597635104e-3849L;
}

/*
** test_longdouble_via_vgmh:
**     vgmh	(%v[0-9]+),1,14
**     vst	\1,0\(%r2\),3
**     br	%r14
*/

long double
test_longdouble_via_vgmh (void)
{
  return 8.9228500591371968978175957554634715383668519805586e+4931L;
}

/*
** test_longdouble_via_vgmf:
**     vgmf	(%v[0-9]+),9,30
**     vst	\1,0\(%r2\),3
**     br	%r14
*/

long double
test_longdouble_via_vgmf (void)
{
  return 5.7202348769040302108562404806917908642856158381792e-4894L;
}

/*
** test_longdouble_via_vgmg:
**     vgmg	(%v[0-9]+),9,62
**     vst	\1,0\(%r2\),3
**     br	%r14
*/

long double
test_longdouble_via_vgmg (void)
{
  return 5.7203220768525291179165318133287569460629228746232e-4894L;
}

/* PR c/102989 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 135
__attribute__((noipa)) void
test135 (_BitInt(135) *p)
{
  *p = 18514202188641047858083570207027263585014wb;
}
#endif

#if __BITINT_MAXWIDTH__ >= 512
__attribute__((noipa)) void
test512 (_BitInt(512) *p)
{
  *p = -3850276968594657220525735372507394006353780328347442833283734788531702263728014201878916354908375176401574990646289191313473197873422312093114382157952958wb;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 135
  _BitInt(135) b135;
  test135 (&b135);
  if (b135 != 18514202188641047858083570207027263585014wb)
    __builtin_abort ();
#endif
#if __BITINT_MAXWIDTH__ >= 512
  _BitInt(512) b512;
  test512 (&b512);
  if (b512 != -3850276968594657220525735372507394006353780328347442833283734788531702263728014201878916354908375176401574990646289191313473197873422312093114382157952958wb)
    __builtin_abort ();
#endif
}

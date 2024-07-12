/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target s390_vxe } */
/* { dg-options "-O2 -march=z14 -mzarch -fdump-tree-optimized" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Test loading constants which cannot be loaded via VECTOR GENERATE BYTE MASK
   nor via VECTOR REPLICATE IMMEDIATE but via VECTOR GENERATE MASK.  */

typedef long double v1tf __attribute__ ((vector_size (16)));

/*
** test_v1tf_via_vgmb:
**     vgm	%v24,4,6,0
**     br	%r14
*/

v1tf
test_v1tf_via_vgmb (void)
{
  return (v1tf){2.263171865473961260249112278523378513150597635104e-3849L};
}

/*
** test_v1tf_via_vgmh:
**     vgm	%v24,1,14,1
**     br	%r14
*/

v1tf
test_v1tf_via_vgmh (void)
{
  return (v1tf){8.9228500591371968978175957554634715383668519805586e+4931L};
}

/*
** test_v1tf_via_vgmf:
**     vgm	%v24,9,30,2
**     br	%r14
*/

v1tf
test_v1tf_via_vgmf (void)
{
  return (v1tf){5.7202348769040302108562404806917908642856158381792e-4894L};
}

/*
** test_v1tf_via_vgmg:
**     vgm	%v24,9,62,3
**     br	%r14
*/

v1tf
test_v1tf_via_vgmg (void)
{
  return (v1tf){5.7203220768525291179165318133287569460629228746232e-4894L};
}

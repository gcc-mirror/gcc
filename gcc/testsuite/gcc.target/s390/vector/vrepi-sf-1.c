/* { dg-do compile } */
/* { dg-require-effective-target s390_vxe } */
/* { dg-options "-O2 -march=z14 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Test loading constants which cannot be loaded via VECTOR GENERATE BYTE MASK
   nor via VECTOR GENERATE MASK but via VECTOR REPLICATE IMMEDIATE.  */

typedef float v1sf __attribute__ ((vector_size (4)));
typedef float v2sf __attribute__ ((vector_size (8)));
typedef float v4sf __attribute__ ((vector_size (16)));

/*
** test_v1sf_via_vrepib:
**     vrepi	%v24,-86,0
**     br	%r14
*/

v1sf
test_v1sf_via_vrepib (void)
{
  return (v1sf){-3.03164883e-13f};
}

/*
** test_v2sf_via_vrepib:
**     vrepi	%v24,-86,0
**     br	%r14
*/

v2sf
test_v2sf_via_vrepib (void)
{
  return (v2sf){-3.03164883e-13f, -3.03164883e-13f};
}

/*
** test_v4sf_via_vrepib:
**     vrepi	%v24,-86,0
**     br	%r14
*/

v4sf
test_v4sf_via_vrepib (void)
{
  return (v4sf){-3.03164883e-13f, -3.03164883e-13f, -3.03164883e-13f, -3.03164883e-13f};
}

/*
** test_v1sf_via_vrepih:
**     vrepi	%v24,-16643,1
**     br	%r14
*/

v1sf
test_v1sf_via_vrepih (void)
{
  return (v1sf){-0.49559775f};
}

/*
** test_v2sf_via_vrepih:
**     vrepi	%v24,-16643,1
**     br	%r14
*/

v2sf
test_v2sf_via_vrepih (void)
{
  return (v2sf){-0.49559775f, -0.49559775f};
}

/*
** test_v4sf_via_vrepih:
**     vrepi	%v24,-16643,1
**     br	%r14
*/

v4sf
test_v4sf_via_vrepih (void)
{
  return (v4sf){-0.49559775f, -0.49559775f, -0.49559775f, -0.49559775f};
}

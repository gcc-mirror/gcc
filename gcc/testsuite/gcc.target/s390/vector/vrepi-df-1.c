/* { dg-do compile } */
/* { dg-require-effective-target s390_vx } */
/* { dg-options "-O2 -march=z13 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Test loading constants which cannot be loaded via VECTOR GENERATE BYTE MASK
   nor via VECTOR GENERATE MASK but via VECTOR REPLICATE IMMEDIATE.  */

typedef double v1df __attribute__ ((vector_size (8)));
typedef double v2df __attribute__ ((vector_size (16)));

/*
** test_v1df_via_vrepib:
**     vrepi	%v24,-86,0
**     br	%r14
*/

v1df
test_v1df_via_vrepib (void)
{
  return (v1df){-3.7206620809969885e-103};
}

/*
** test_v2df_via_vrepib:
**     vrepi	%v24,-86,0
**     br	%r14
*/

v2df
test_v2df_via_vrepib (void)
{
  return (v2df){-3.7206620809969885e-103, -3.7206620809969885e-103};
}

/*
** test_v1df_via_vrepih:
**     vrepi	%v24,-16643,1
**     br	%r14
*/

v1df
test_v1df_via_vrepih (void)
{
  return (v1df){-2.8368052823634315e-05};
}

/*
** test_v2df_via_vrepih:
**     vrepi	%v24,-16643,1
**     br	%r14
*/

v2df
test_v2df_via_vrepih (void)
{
  return (v2df){-2.8368052823634315e-05, -2.8368052823634315e-05};
}

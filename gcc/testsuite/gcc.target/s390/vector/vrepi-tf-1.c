/* { dg-do compile } */
/* { dg-require-effective-target s390_vxe } */
/* { dg-options "-O2 -march=z14 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Test loading constants which cannot be loaded via VECTOR GENERATE BYTE MASK
   nor via VECTOR GENERATE MASK but via VECTOR REPLICATE IMMEDIATE.  */

typedef long double v1tf __attribute__ ((vector_size (16)));

/*
** test_v1tf_via_vrepib:
**     vrepi	%v24,-86,0
**     br	%r14
*/

v1tf
test_v1tf_via_vrepib (void)
{
  return (v1tf){-1.98172062152833090752940986271726055e-1644L};
}

/*
** test_v1tf_via_vrepih:
**     vrepi	%v24,-16643,1
**     br	%r14
*/

v1tf
test_v1tf_via_vrepih (void)
{
  return (v1tf){-3.76981572984797096816094251528390952e-78L};
}

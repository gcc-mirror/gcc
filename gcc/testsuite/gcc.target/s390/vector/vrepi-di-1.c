/* { dg-do compile } */
/* { dg-require-effective-target s390_vx } */
/* { dg-options "-O2 -march=z13 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Test loading constants which cannot be loaded via VECTOR GENERATE BYTE MASK
   nor via VECTOR GENERATE MASK but via VECTOR REPLICATE IMMEDIATE.  */

typedef long long v1di __attribute__ ((vector_size (8)));
typedef long long v2di __attribute__ ((vector_size (16)));

/*
** test_v1di_via_vrepib:
**     vrepi	%v24,-86,0
**     br	%r14
*/

v1di
test_v1di_via_vrepib (void)
{
  return (v1di){0xaaaaaaaaaaaaaaaa};
}

/*
** test_v2di_via_vrepib:
**     vrepi	%v24,-86,0
**     br	%r14
*/

v2di
test_v2di_via_vrepib (void)
{
  return (v2di){0xaaaaaaaaaaaaaaaa, 0xaaaaaaaaaaaaaaaa};
}

/*
** test_v1di_via_vrepih:
**     vrepi	%v24,-16643,1
**     br	%r14
*/

v1di
test_v1di_via_vrepih (void)
{
  return (v1di){0xbefdbefdbefdbefd};
}

/*
** test_v2di_via_vrepih:
**     vrepi	%v24,-16643,1
**     br	%r14
*/

v2di
test_v2di_via_vrepih (void)
{
  return (v2di){0xbefdbefdbefdbefd, 0xbefdbefdbefdbefd};
}

/*
** test_v1di_via_vrepif:
**     vrepi	%v24,-16643,2
**     br	%r14
*/

v1di
test_v1di_via_vrepif (void)
{
  return (v1di){0xffffbefdffffbefd};
}

/*
** test_v2di_via_vrepif:
**     vrepi	%v24,-16643,2
**     br	%r14
*/

v2di
test_v2di_via_vrepif (void)
{
  return (v2di){0xffffbefdffffbefd, 0xffffbefdffffbefd};
}

/*
** test_v1di_via_vrepig:
**     vrepi	%v24,-16643,3
**     br	%r14
*/

v1di
test_v1di_via_vrepig (void)
{
  return (v1di){-16643};
}

/*
** test_v2di_via_vrepig:
**     vrepi	%v24,-16643,3
**     br	%r14
*/

v2di
test_v2di_via_vrepig (void)
{
  return (v2di){-16643, -16643};
}

/* { dg-do compile } */
/* { dg-require-effective-target s390_vx } */
/* { dg-options "-O2 -march=z13 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Test loading constants which cannot be loaded via VECTOR GENERATE BYTE MASK
   nor via VECTOR GENERATE MASK but via VECTOR REPLICATE IMMEDIATE.  */

typedef int v1si __attribute__ ((vector_size (4)));
typedef int v2si __attribute__ ((vector_size (8)));
typedef int v4si __attribute__ ((vector_size (16)));

/*
** test_v1si_via_vrepib:
**     vrepi	%v24,-86,0
**     br	%r14
*/

v1si
test_v1si_via_vrepib (void)
{
  return (v1si){0xaaaaaaaa};
}

/*
** test_v2si_via_vrepib:
**     vrepi	%v24,-86,0
**     br	%r14
*/

v2si
test_v2si_via_vrepib (void)
{
  return (v2si){0xaaaaaaaa, 0xaaaaaaaa};
}

/*
** test_v4si_via_vrepib:
**     vrepi	%v24,-86,0
**     br	%r14
*/

v4si
test_v4si_via_vrepib (void)
{
  return (v4si){0xaaaaaaaa, 0xaaaaaaaa, 0xaaaaaaaa, 0xaaaaaaaa};
}

/*
** test_v1si_via_vrepih:
**     vrepi	%v24,-16643,1
**     br	%r14
*/

v1si
test_v1si_via_vrepih (void)
{
  return (v1si){-1090666755};
}

/*
** test_v2si_via_vrepih:
**     vrepi	%v24,-16643,1
**     br	%r14
*/

v2si
test_v2si_via_vrepih (void)
{
  return (v2si){-1090666755, -1090666755};
}

/*
** test_v4si_via_vrepih:
**     vrepi	%v24,-16643,1
**     br	%r14
*/

v4si
test_v4si_via_vrepih (void)
{
  return (v4si){-1090666755, -1090666755, -1090666755, -1090666755};
}

/*
** test_v1si_via_vrepif:
**     vrepi	%v24,-16643,2
**     br	%r14
*/

v1si
test_v1si_via_vrepif (void)
{
  return (v1si){-16643};
}

/*
** test_v2si_via_vrepif:
**     vrepi	%v24,-16643,2
**     br	%r14
*/

v2si
test_v2si_via_vrepif (void)
{
  return (v2si){-16643, -16643};
}

/*
** test_v4si_via_vrepif:
**     vrepi	%v24,-16643,2
**     br	%r14
*/

v4si
test_v4si_via_vrepif (void)
{
  return (v4si){-16643, -16643, -16643, -16643};
}

/*
** test_v2si_via_vrepig:
**     vrepi	%v24,-16643,3
**     br	%r14
*/

v2si
test_v2si_via_vrepig (void)
{
  return (v2si){-1, -16643};
}

/*
** test_v4si_via_vrepig:
**     vrepi	%v24,-16643,3
**     br	%r14
*/

v4si
test_v4si_via_vrepig (void)
{
  return (v4si){-1, -16643, -1, -16643};
}

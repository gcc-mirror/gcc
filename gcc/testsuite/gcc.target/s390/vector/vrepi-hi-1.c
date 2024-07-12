/* { dg-do compile } */
/* { dg-require-effective-target s390_vx } */
/* { dg-options "-O2 -march=z13 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Test loading constants which cannot be loaded via VECTOR GENERATE BYTE MASK
   nor via VECTOR GENERATE MASK but via VECTOR REPLICATE IMMEDIATE.  */

typedef short v1hi __attribute__ ((vector_size (2)));
typedef short v2hi __attribute__ ((vector_size (4)));
typedef short v4hi __attribute__ ((vector_size (8)));
typedef short v8hi __attribute__ ((vector_size (16)));

/*
** test_v1hi_via_vrepib:
**     vrepi	%v24,-86,0
**     br	%r14
*/

v1hi
test_v1hi_via_vrepib (void)
{
  return (v1hi){0xaaaa};
}

/*
** test_v2hi_via_vrepib:
**     vrepi	%v24,-86,0
**     br	%r14
*/

v2hi
test_v2hi_via_vrepib (void)
{
  return (v2hi){0xaaaa, 0xaaaa};
}

/*
** test_v4hi_via_vrepib:
**     vrepi	%v24,-86,0
**     br	%r14
*/

v4hi
test_v4hi_via_vrepib (void)
{
  return (v4hi){0xaaaa, 0xaaaa, 0xaaaa, 0xaaaa};
}

/*
** test_v8hi_via_vrepib:
**     vrepi	%v24,-86,0
**     br	%r14
*/

v8hi
test_v8hi_via_vrepib (void)
{
  return (v8hi){0xaaaa, 0xaaaa, 0xaaaa, 0xaaaa, 0xaaaa, 0xaaaa, 0xaaaa, 0xaaaa};
}

/*
** test_v1hi_via_vrepih:
**     vrepi	%v24,-16643,1
**     br	%r14
*/

v1hi
test_v1hi_via_vrepih (void)
{
  return (v1hi){-16643};
}

/*
** test_v2hi_via_vrepih:
**     vrepi	%v24,-16643,1
**     br	%r14
*/

v2hi
test_v2hi_via_vrepih (void)
{
  return (v2hi){-16643, -16643};
}

/*
** test_v4hi_via_vrepih:
**     vrepi	%v24,-16643,1
**     br	%r14
*/

v4hi
test_v4hi_via_vrepih (void)
{
  return (v4hi){-16643, -16643, -16643, -16643};
}

/*
** test_v8hi_via_vrepih:
**     vrepi	%v24,-16643,1
**     br	%r14
*/

v8hi
test_v8hi_via_vrepih (void)
{
  return (v8hi){-16643, -16643, -16643, -16643, -16643, -16643, -16643, -16643};
}

/*
** test_v2hi_via_vrepif:
**     vrepi	%v24,-16643,2
**     br	%r14
*/

v2hi
test_v2hi_via_vrepif (void)
{
  return (v2hi){-1, -16643};
}

/*
** test_v4hi_via_vrepif:
**     vrepi	%v24,-16643,2
**     br	%r14
*/

v4hi
test_v4hi_via_vrepif (void)
{
  return (v4hi){-1, -16643, -1, -16643};
}

/*
** test_v8hi_via_vrepif:
**     vrepi	%v24,-16643,2
**     br	%r14
*/

v8hi
test_v8hi_via_vrepif (void)
{
  return (v8hi){-1, -16643, -1, -16643, -1, -16643, -1, -16643};
}

/*
** test_v4hi_via_vrepig:
**     vrepi	%v24,-16643,3
**     br	%r14
*/

v4hi
test_v4hi_via_vrepig (void)
{
  return (v4hi){-1, -1, -1, -16643};
}

/*
** test_v8hi_via_vrepig:
**     vrepi	%v24,-16643,3
**     br	%r14
*/

v8hi
test_v8hi_via_vrepig (void)
{
  return (v8hi){-1, -1, -1, -16643, -1, -1, -1, -16643};
}

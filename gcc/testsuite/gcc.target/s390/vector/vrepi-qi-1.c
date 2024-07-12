/* { dg-do compile } */
/* { dg-require-effective-target s390_vx } */
/* { dg-options "-O2 -march=z13 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Test loading constants which cannot be loaded via VECTOR GENERATE BYTE MASK
   nor via VECTOR GENERATE MASK but via VECTOR REPLICATE IMMEDIATE.  */

typedef signed char v1qi __attribute__ ((vector_size (1)));
typedef signed char v2qi __attribute__ ((vector_size (2)));
typedef signed char v4qi __attribute__ ((vector_size (4)));
typedef signed char v8qi __attribute__ ((vector_size (8)));
typedef signed char v16qi __attribute__ ((vector_size (16)));

/*
** test_v1qi_via_vrepib:
**     vrepi	%v24,-86,0
**     br	%r14
*/

v1qi
test_v1qi_via_vrepib (void)
{
  return (v1qi){0xaa};
}

/*
** test_v2qi_via_vrepib:
**     vrepi	%v24,-86,0
**     br	%r14
*/

v2qi
test_v2qi_via_vrepib (void)
{
  return (v2qi){0xaa, 0xaa};
}

/*
** test_v4qi_via_vrepib:
**     vrepi	%v24,-86,0
**     br	%r14
*/

v4qi
test_v4qi_via_vrepib (void)
{
  return (v4qi){0xaa, 0xaa, 0xaa, 0xaa};
}

/*
** test_v8qi_via_vrepib:
**     vrepi	%v24,-86,0
**     br	%r14
*/

v8qi
test_v8qi_via_vrepib (void)
{
  return (v8qi){0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa};
}

/*
** test_v16qi_via_vrepib:
**     vrepi	%v24,-86,0
**     br	%r14
*/

v16qi
test_v16qi_via_vrepib (void)
{
  return (v16qi){0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa};
}

/*
** test_v2qi_via_vrepih:
**     vrepi	%v24,-16643,1
**     br	%r14
*/

v2qi
test_v2qi_via_vrepih (void)
{
  return (v2qi){0xbe, 0xfd};
}

/*
** test_v4qi_via_vrepih:
**     vrepi	%v24,-16643,1
**     br	%r14
*/

v4qi
test_v4qi_via_vrepih (void)
{
  return (v4qi){0xbe, 0xfd, 0xbe, 0xfd};
}

/*
** test_v8qi_via_vrepih:
**     vrepi	%v24,-16643,1
**     br	%r14
*/

v8qi
test_v8qi_via_vrepih (void)
{
  return (v8qi){0xbe, 0xfd, 0xbe, 0xfd, 0xbe, 0xfd, 0xbe, 0xfd};
}

/*
** test_v16qi_via_vrepih:
**     vrepi	%v24,-16643,1
**     br	%r14
*/

v16qi
test_v16qi_via_vrepih (void)
{
  return (v16qi){0xbe, 0xfd, 0xbe, 0xfd, 0xbe, 0xfd, 0xbe, 0xfd, 0xbe, 0xfd, 0xbe, 0xfd, 0xbe, 0xfd, 0xbe, 0xfd};
}

/*
** test_v16qi_via_vrepif:
**     vrepi	%v24,-16643,2
**     br	%r14
*/

v16qi
test_v16qi_via_vrepif (void)
{
  return (v16qi){-1, -1, 0xbe, 0xfd, -1, -1, 0xbe, 0xfd, -1, -1, 0xbe, 0xfd, -1, -1, 0xbe, 0xfd};
}

/*
** test_v16qi_via_vrepig:
**     vrepi	%v24,-16643,3
**     br	%r14
*/

v16qi
test_v16qi_via_vrepig (void)
{
  return (v16qi){-1, -1, -1, -1, -1, -1, 0xbe, 0xfd, -1, -1, -1, -1, -1, -1, 0xbe, 0xfd};
}

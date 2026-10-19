/* { dg-do compile } */
/* { dg-options "-O2 -march=z13" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Basic tests for VMAL where all operands are in registers (except for the
   scalar TI mode test).  VMALG/Q become available with TARGET_VXE3/z17.  */

typedef signed char v16qi __attribute__ ((vector_size (16)));
typedef short v8hi __attribute__ ((vector_size (16)));
typedef int v4si __attribute__ ((vector_size (16)));
typedef long v2di __attribute__ ((vector_size (16)));
typedef __int128 v1ti __attribute__ ((vector_size (16)));

/*
** test_v16qi:
**	vmalb	%v24,%v26,%v28,%v24
**	br	%r14
*/

v16qi test_v16qi (v16qi a, v16qi b, v16qi c)
{
  return a + b * c;
}

/*
** test_v8hi:
**	vmalhw	%v24,%v26,%v28,%v24
**	br	%r14
*/

v8hi test_v8hi (v8hi a, v8hi b, v8hi c)
{
  return a + b * c;
}

/*
** test_v4si:
**	vmalf	%v24,%v26,%v28,%v24
**	br	%r14
*/

v4si test_v4si (v4si a, v4si b, v4si c)
{
  return a + b * c;
}

/*
** test_v2di:
**	vmalg	%v24,%v26,%v28,%v24
**	br	%r14
*/

__attribute__ ((target ("arch=z17")))
v2di test_v2di (v2di a, v2di b, v2di c)
{
  return a + b * c;
}

/*
** test_v1ti:
**	vmalq	%v24,%v26,%v28,%v24
**	br	%r14
*/

__attribute__ ((target ("arch=z17")))
v1ti test_v1ti (v1ti a, v1ti b, v1ti c)
{
  return a + b * c;
}

/*
** test_ti:
**	vl	.*
**	vl	.*
**	vl	.*
**	vmalq	.*
**	vst	.*
**	br	%r14
*/

__attribute__ ((target ("arch=z17")))
__int128 test_ti (__int128 a, __int128 b, __int128 c)
{
  return a + b * c;
}

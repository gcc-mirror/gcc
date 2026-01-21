/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-final { check-function-bodies "**" "" "" } } */

typedef __int128 __attribute__ ((vector_size (16))) V1TI;
typedef unsigned __int128 __attribute__ ((vector_size (16))) UV1TI;

/*
** cmpeq:
**	vceqq	%v24,%v24,%v26
**	br	%r14
*/

V1TI
cmpeq (V1TI x, V1TI y)
{
  return x == y;
}

/*
** cmpgt:
**	vchq	%v24,%v24,%v26
**	br	%r14
*/

V1TI
cmpgt (V1TI x, V1TI y)
{
  return x > y;
}

/*
** cmpgtu:
**	vchlq	%v24,%v24,%v26
**	br	%r14
*/

V1TI
cmpgtu (UV1TI x, UV1TI y)
{
  return x > y;
}

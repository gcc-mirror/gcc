/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

typedef char v16qi __attribute__ ((vector_size (16)));
typedef char v8qi __attribute__ ((vector_size (8)));
typedef short v8hi __attribute__ ((vector_size (16)));
typedef short v4hi __attribute__ ((vector_size (8)));
typedef int v4si __attribute__ ((vector_size (16)));
typedef int v2si __attribute__ ((vector_size (8)));
typedef long v2di __attribute__ ((vector_size (16)));

/*
** f_v16qi:
**	index	z0\.b, #0, #1
**	ret
*/
v16qi
f_v16qi (void)
{
  return (v16qi){ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
}

/*
** f_v8qi:
**	index	z0\.b, #0, #1
**	ret
*/
v8qi
f_v8qi (void)
{
  return (v8qi){ 0, 1, 2, 3, 4, 5, 6, 7 };
}

/*
** f_v8hi:
**	index	z0\.h, #0, #1
**	ret
*/
v8hi
f_v8hi (void)
{
  return (v8hi){ 0, 1, 2, 3, 4, 5, 6, 7 };
}

/*
** f_v4hi:
**	index	z0\.h, #0, #1
**	ret
*/
v4hi
f_v4hi (void)
{
  return (v4hi){ 0, 1, 2, 3 };
}

/*
** f_v4si:
**	index	z0\.s, #0, #1
**	ret
*/
v4si
f_v4si (void)
{
  return (v4si){ 0, 1, 2, 3 };
}

/*
** f_v2si:
**	index	z0\.s, #0, #1
**	ret
*/
v2si
f_v2si (void)
{
  return (v2si){ 0, 1 };
}

/*
** f_v2di:
**	index	z0\.d, #0, #1
**	ret
*/
v2di
f_v2di (void)
{
  return (v2di){ 0, 1 };
}

/*
** g_v4si:
**	index	z0\.s, #3, #-4
**	ret
*/
v4si
g_v4si (void)
{
  return (v4si){ 3, -1, -5, -9 };
}

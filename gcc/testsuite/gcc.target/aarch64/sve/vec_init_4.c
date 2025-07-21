/* { dg-do compile } */
/* { dg-options "-O2 -mbig-endian" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

typedef char v16qi __attribute__ ((vector_size (16)));
typedef char v8qi __attribute__ ((vector_size (8)));
typedef short v8hi __attribute__ ((vector_size (16)));
typedef short v4hi __attribute__ ((vector_size (8)));
typedef int v4si __attribute__ ((vector_size (16)));
typedef int v2si __attribute__ ((vector_size (8)));
typedef long long v2di __attribute__ ((vector_size (16)));

/*
** f_v16qi:
**	index	z0\.b, #15, #-1
**	ret
*/
v16qi
f_v16qi (void)
{
  return (v16qi){ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
}

/*
** f_v8qi:
**	index	z0\.b, #7, #-1
**	ret
*/
v8qi
f_v8qi (void)
{
  return (v8qi){ 0, 1, 2, 3, 4, 5, 6, 7 };
}

/*
** f_v8hi:
**	index	z0\.h, #7, #-1
**	ret
*/
v8hi
f_v8hi (void)
{
  return (v8hi){ 0, 1, 2, 3, 4, 5, 6, 7 };
}

/*
** f_v4hi:
**	index	z0\.h, #3, #-1
**	ret
*/
v4hi
f_v4hi (void)
{
  return (v4hi){ 0, 1, 2, 3 };
}

/*
** f_v4si:
**	index	z0\.s, #3, #-1
**	ret
*/
v4si
f_v4si (void)
{
  return (v4si){ 0, 1, 2, 3 };
}

/*
** f_v2si:
**	index	z0\.s, #1, #-1
**	ret
*/
v2si
f_v2si (void)
{
  return (v2si){ 0, 1 };
}

/*
** f_v2di:
**	index	z0\.d, #1, #-1
**	ret
*/
v2di
f_v2di (void)
{
  return (v2di){ 0, 1 };
}

/*
** g_v4si:
**	index	z0\.s, #-9, #4
**	ret
*/
v4si
g_v4si (void)
{
  return (v4si){ 3, -1, -5, -9 };
}

/*
** g_min_1:
**	index	z0\.s, #-16, #1
**	ret
*/
v4si
g_min_1 (void)
{
  return (v4si){ -13, -14, -15, -16 };
}

/*
** g_min_min:
**	index	z0\.s, #-16, #-16
**	ret
*/
v4si
g_min_min (void)
{
  return (v4si){ -64, -48, -32, -16 };
}

/*
** g_min_max:
**	index	z0\.s, #-16, #15
**	ret
*/
v4si
g_min_max (void)
{
  return (v4si){ 29, 14, -1, -16 };
}

/*
** g_max_1:
**	index	z0\.s, #15, #1
**	ret
*/
v4si
g_max_1 (void)
{
  return (v4si){ 18, 17, 16, 15 };
}

/*
** g_max_min:
**	index	z0\.s, #15, #-16
**	ret
*/
v4si
g_max_min (void)
{
  return (v4si){ -33, -17, -1, 15 };
}

/*
** g_max_max:
**	index	z0\.s, #15, #15
**	ret
*/
v4si
g_max_max (void)
{
  return (v4si){ 60, 45, 30, 15 };
}

/*
** g_ob_1:
**	((?!index).)*
**	ret
*/
v4si
g_ob_1 (void)
{
  return (v4si){ -14, -15, -16, -17 };
}

/*
** g_ob_2:
**	((?!index).)*
**	ret
*/
v4si
g_ob_2 (void)
{
  return (v4si){ 19, 18, 17, 16 };
}

/*
** g_ob_3:
**	((?!index).)*
**	ret
*/
v4si
g_ob_3 (void)
{
  return (v4si){ -51, -34, -17, 0 };
}

/*
** g_ob_4:
**	((?!index).)*
**	ret
*/
v4si
g_ob_4 (void)
{
  return (v4si){ 48, 32, 16, 0 };
}

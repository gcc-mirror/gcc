/* { dg-do compile } */
/* { dg-options "-O2 -march=z13" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* For halfwords a byte-swap is a rotate by 8.  */

typedef unsigned char v16qi __attribute__ ((vector_size (16)));

/*
** test_reg_reg:
**	verllh	%v24,%v24,8
**	br	%r14
*/
v16qi
test_reg_reg (v16qi x)
{
  return __builtin_shuffle (x, (v16qi) { 1, 0, 3, 2, 5, 4, 7, 6,
					 9, 8, 11, 10, 13, 12, 15, 14 });
}

/*
** test_reg_mem:
**	vl	%v24,0\(%r2\),3
**	verllh	%v24,%v24,8
**	br	%r14
*/
v16qi
test_reg_mem (v16qi *x)
{
  return __builtin_shuffle (*x, (v16qi) { 1, 0, 3, 2, 5, 4, 7, 6,
					  9, 8, 11, 10, 13, 12, 15, 14 });
}

/*
** test_mem_reg:
**	verllh	%v24,%v24,8
**	vst	%v24,0\(%r2\),3
**	br	%r14
*/
void
test_mem_reg (v16qi *x, v16qi y)
{
  *x = __builtin_shuffle (y, (v16qi) { 1, 0, 3, 2, 5, 4, 7, 6,
				       9, 8, 11, 10, 13, 12, 15, 14 });
}

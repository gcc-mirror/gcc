/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mrvv-vector-bits=zvl -ffast-math -mabi=lp64 -O3" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"

/*
**test_1:
**	...
**	frrm\t[axt][0-9]+
**	...
**	fsrmi\t1
**	...
**	vfsub\.vv\tv[0-9]+,v[0-9]+,v[0-9]+
**	...
**	fsrm\t[axt][0-9]+
**	...
**	vfmadd\.vv\tv[0-9]+,v[0-9]+,v[0-9]+
**	...
**	ret
*/
void
test_1 (vfloat32m1_t op1, vfloat32m1_t op2, vfloat32m1_t *op_out, size_t vl,
	double *in1, double *in2, double *out)
{
  *op_out = __riscv_vfsub_vv_f32m1_rm (op1, op2, 1, vl);

  for (int i = 0; i < 4; ++i)
    out[i] += in1[i] * in2[i];
}

/*
**test_2:
**	...
**	frrm\t[axt][0-9]+
**	...
**	fsrmi\t1
**	...
**	vfsub\.vv\tv[0-9]+,v[0-9]+,v[0-9]+
**	...
**	fsrm\t[axt][0-9]+
**	...
**	vfmadd\.vv\tv[0-9]+,v[0-9]+,v[0-9]+
**	...
**	fsrmi\t4
**	...
**	vfsub\.vv\tv[0-9]+,v[0-9]+,v[0-9]+
**	...
**	fsrm\t[axt][0-9]+
**	...
**	ret
*/
void
test_2 (vfloat32m1_t op1, vfloat32m1_t op2, vfloat32m1_t *op_out, size_t vl,
	double *in1, double *in2, double *out)
{
  op2 = __riscv_vfsub_vv_f32m1_rm (op1, op2, 1, vl);

  for (int i = 0; i < 4; ++i)
    out[i] = out[i] * in1[i] + in2[i];

  *op_out = __riscv_vfsub_vv_f32m1_rm (op1, op2, 4, vl);
}

/*
**test_3:
**	...
**	frrm\t[axt][0-9]+
**	...
**	vfmadd\.vv\tv[0-9]+,v[0-9]+,v[0-9]+
**	...
**	fsrmi\t4
**	...
**	vfsub\.vv\tv[0-9]+,v[0-9]+,v[0-9]+
**	...
**	fsrm\t[axt][0-9]+
**	...
**	ret
*/
void
test_3 (vfloat32m1_t op1, vfloat32m1_t op2, vfloat32m1_t *op_out, size_t vl,
	double *in1, double *in2, double *in3, double *out)
{
  for (int i = 0; i < 4; ++i)
    out[i] = in1[i] + in2[i] * out[i];

  *op_out = __riscv_vfsub_vv_f32m1_rm (op1, op2, 4, vl);
}

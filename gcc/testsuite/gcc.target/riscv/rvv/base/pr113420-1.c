/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

void
matrix_transpose_intrinsics (float *dst, float *src, size_t n)
{
  for (size_t row_id = 0; row_id < n; ++row_id)
    { // input row-index
      size_t avl = n;
      // source pointer to row_id-th row
      float *row_src = src + row_id * n;
      // destination pointer to row_id-th column
      float *row_dst = dst + row_id;
      while (avl > 0)
	{
	  size_t vl = __riscv_vsetvl_e32m1 (avl);
	  vfloat32m1_t row = __riscv_vle32_v_f32m1 (row_src, vl);
	  __riscv_vsse32 (row_dst, sizeof (float) * n, row, vl);
	  // updating application vector length
	  avl -= vl;
	  // updating source and destination pointers
	  row_src += vl;
	  row_dst += vl * n;
	}
    }
}

/* { dg-final { scan-assembler-times {vsse32\.v} 1 } } */

/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl128b -mabi=lp64 -mrvv-vector-bits=scalable -O3" } */

#include "riscv_vector.h"

void test_rvv_vector_bits_scalable ()
{
  vint32m1_t x;
  asm volatile ("def %0": "=vr"(x));
  asm volatile (""::: "v0",   "v1",  "v2",  "v3",  "v4",  "v5",  "v6",  "v7",
		      "v8",   "v9", "v10", "v11", "v12", "v13", "v14", "v15",
		      "v16", "v17", "v18", "v19", "v20", "v21", "v22", "v23",
		      "v24", "v25", "v26", "v27", "v28", "v29", "v30", "v31");
  asm volatile ("use %0": : "vr"(x));
}

/* { dg-final { scan-assembler-times {csrr\s+[atx][0-9]+,\s*vlenb} 2 } } */

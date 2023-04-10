/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3" } */

#include "riscv_vector.h"

void f (void *base1,void *base2,void *base3,void *base4,void *out,size_t vl, uint16_t shift)
{
    vuint16m1_t v1 = __riscv_vle16_v_u16m1 (base1, vl);
    vbool16_t m1 = __riscv_vlm_v_b16 (base3, vl);
    vbool16_t m2 = __riscv_vlm_v_b16 (base4, vl);
    asm volatile("#" ::
		 : "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29");

    vbool16_t v = __riscv_vmsltu_vx_u16m1_b16_mu(m1,m2,v1,shift,vl);
    asm volatile("#" ::
		 : "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v30", "v31");


    __riscv_vsm_v_b16 (out,v,vl);
}

/* { dg-final { scan-assembler-times {vmv} 1 } } */

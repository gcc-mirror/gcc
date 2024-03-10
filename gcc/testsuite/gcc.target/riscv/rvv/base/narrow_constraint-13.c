/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3" } */

#include "riscv_vector.h"

void f0 (int16_t *base,int8_t *out,size_t vl)
{
    vint16m2_t v0 = __riscv_vle16_v_i16m2 (base, vl);
    vint16m2_t v1 = __riscv_vle16_v_i16m2 ((int16_t *)(base + 100), vl);
    vbool8_t m = __riscv_vlm_v_b8 ((uint8_t *)(base + 200), vl);
    asm volatile("#" ::
		 : "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27");

    m = __riscv_vmadc_vvm_i16m2_b8 (v0, v1, m, 4);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v29", "v30", "v31");

    __riscv_vsm_v_b8 (out,m,vl);
}

void f1 (int16_t *base,int8_t *out,size_t vl)
{
    vint16m2_t v0 = __riscv_vle16_v_i16m2 (base, vl);
    vint16m2_t v1 = __riscv_vle16_v_i16m2 ((int16_t *)(base + 100), vl);
    vbool8_t m = __riscv_vlm_v_b8 ((uint8_t *)(base + 200), vl);
    asm volatile("#" ::
		 : "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27");

    m = __riscv_vmadc_vvm_i16m2_b8 (v0, v1, m, 4);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v31");

    __riscv_vsm_v_b8 (out,m,vl);
}

void f2 (int16_t *base,int8_t *out,size_t vl)
{
    vint16m2_t v0 = __riscv_vle16_v_i16m2 (base, vl);
    vint16m2_t v1 = __riscv_vle16_v_i16m2 ((int16_t *)(base + 100), vl);
    vbool8_t m = __riscv_vlm_v_b8 ((uint8_t *)(base + 200), vl);
    asm volatile("#" ::
		 : "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27");

    m = __riscv_vmadc_vvm_i16m2_b8 (v0, v1, m, 4);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v30", "v31");

    __riscv_vsm_v_b8 (out,m,vl);
}

void f3 (int16_t *base,int8_t *out,size_t vl)
{
    vint16mf2_t v0 = __riscv_vle16_v_i16mf2 (base, vl);
    vint16mf2_t v1 = __riscv_vle16_v_i16mf2 ((int16_t *)(base + 100), vl);
    vbool32_t m = __riscv_vlm_v_b32 ((uint8_t *)(base + 200), vl);
    asm volatile("#" ::
		 : "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29");

    m = __riscv_vmadc_vvm_i16mf2_b32 (v0, v1, m, 4);
    asm volatile("#" ::
		 : "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v30");

    __riscv_vsm_v_b32 (out,m,vl);
}

void f4 (int16_t *base,int8_t *out,size_t vl)
{
    vint16mf2_t v0 = __riscv_vle16_v_i16mf2 (base, vl);
    vint16mf2_t v1 = __riscv_vle16_v_i16mf2 ((int16_t *)(base + 100), vl);
    vbool32_t m = __riscv_vlm_v_b32 ((uint8_t *)(base + 200), vl);
    asm volatile("#" ::
		 : "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29");

    m = __riscv_vmadc_vvm_i16mf2_b32 (v0, v1, m, 4);
    asm volatile("#" ::
		 : "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v31");

    __riscv_vsm_v_b32 (out,m,vl);
}

void f5 (int16_t *base,int8_t *out,size_t vl)
{
    vint16mf2_t v0 = __riscv_vle16_v_i16mf2 (base, vl);
    vint16mf2_t v1 = __riscv_vle16_v_i16mf2 ((int16_t *)(base + 100), vl);
    vbool32_t m = __riscv_vlm_v_b32 ((uint8_t *)(base + 200), vl);
    asm volatile("#" ::
		 : "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29");

    m = __riscv_vmadc_vvm_i16mf2_b32 (v0, v1, m, 4);
    asm volatile("#" ::
		 : "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v30", "v31");

    __riscv_vsm_v_b32 (out,m,vl);
}

/* { dg-final { scan-assembler-not {vmv} } } */
/* { dg-final { scan-assembler-not {csrr} } } */

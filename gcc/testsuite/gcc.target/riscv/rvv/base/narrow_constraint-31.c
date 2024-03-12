/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-sched-pressure" } */

#include "riscv_vector.h"

void f0 (void *base1,void *base2,void *base3,void *base4,void *out,size_t vl, float x)
{
    vfloat32m8_t v1 = __riscv_vle32_v_f32m8 (base1, vl);
    vbool4_t m1 = __riscv_vlm_v_b4 (base3, vl);
    asm volatile("#" ::
		 : "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23","v24","v25",  
		   "v26", "v27", "v28", "v29", "v30", "v31");

    vbool4_t v = __riscv_vmflt_vf_f32m8_b4_m(m1,v1,x,vl);
    asm volatile("#" ::
		 : "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v30", "v31");

    __riscv_vsm_v_b4 (out,v,vl);
}

void f1 (void *base1,void *base2,void *base3,void *base4,void *out,size_t vl, float x)
{
    vfloat32m8_t v1 = __riscv_vle32_v_f32m8 (base1, vl);
    vbool4_t m1 = __riscv_vlm_v_b4 (base3, vl);
    asm volatile("#" ::
		 : "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23","v24","v25",  
		   "v26", "v27", "v28", "v29", "v30", "v31");

    vbool4_t v = __riscv_vmflt_vf_f32m8_b4_mu(m1,m1,v1,x,vl);
    asm volatile("#" ::
		 : "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v30", "v31");

    __riscv_vsm_v_b4 (out,v,vl);
}

void f2 (void *base1,void *base2,void *base3,void *base4,void *out,size_t vl, float x)
{
    vfloat32m8_t v1 = __riscv_vle32_v_f32m8 (base1, vl);
    vbool4_t m1 = __riscv_vlm_v_b4 (base3, vl);
    asm volatile("#" ::
		 : "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23","v24","v25",  
		   "v26", "v27", "v28", "v29", "v30", "v31");

    vbool4_t v = __riscv_vmflt_vf_f32m8_b4_m(m1,v1,x,vl);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v30", "v31");

    __riscv_vsm_v_b4 (out,v,vl);
}

void f3 (void *base1,void *base2,void *base3,void *base4,void *out,size_t vl, float x)
{
    vfloat32m8_t v1 = __riscv_vle32_v_f32m8 (base1, vl);
    vbool4_t m1 = __riscv_vlm_v_b4 (base3, vl);
    asm volatile("#" ::
		 : "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23","v24","v25",  
		   "v26", "v27", "v28", "v29", "v30", "v31");

    vbool4_t v = __riscv_vmflt_vf_f32m8_b4_m(m1,v1,x,vl);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v30", "v31");

    __riscv_vsm_v_b4 (out,v,vl);
}

void f4 (void *base1,void *base2,void *base3,void *base4,void *out,size_t vl, float x)
{
    vfloat32m8_t v1 = __riscv_vle32_v_f32m8 (base1, vl);
    vbool4_t m1 = __riscv_vlm_v_b4 (base3, vl);
    asm volatile("#" ::
		 : "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v30", "v31");

    vbool4_t v = __riscv_vmflt_vf_f32m8_b4_m(m1,v1,x,vl);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v30", "v31");

    __riscv_vsm_v_b4 (out,v,vl);
}

void f5 (void *base1,void *base2,void *base3,void *base4,void *out,size_t vl, float x)
{
    vfloat32m8_t v1 = __riscv_vle32_v_f32m8 (base1, vl);
    vbool4_t m1 = __riscv_vlm_v_b4 (base3, vl);
    asm volatile("#" ::
		 : "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23","v24","v25",  
		   "v26", "v27", "v28", "v29", "v30", "v31");

    vbool4_t v = __riscv_vmflt_vf_f32m8_b4_m(m1,v1,x,vl);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v28", "v29", "v30", "v31");

    __riscv_vsm_v_b4 (out,v,vl);
}

void f6 (void *base1,void *base2,void *base3,void *base4,void *out,size_t vl, float x)
{
    vfloat32m8_t v1 = __riscv_vle32_v_f32m8 (base1, vl);
    vbool4_t m1 = __riscv_vlm_v_b4 (base3, vl);
    vbool4_t m2 = __riscv_vlm_v_b4 (base4, vl);
    asm volatile("#" ::
		 : "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23","v24","v25",  
		   "v26", "v28", "v29", "v30", "v31");

    vbool4_t v = __riscv_vmflt_vf_f32m8_b4_mu(m1,m2,v1,x,vl);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v28", "v29", "v30", "v31");

    __riscv_vsm_v_b4 (out,v,vl);
}

void f7 (void * in, void *out, float x)
{
    vbool4_t mask = *(vbool4_t*)in;
    asm volatile ("":::"memory");
    vfloat32m8_t v = __riscv_vle32_v_f32m8 (in, 4);
    vfloat32m8_t v2 = __riscv_vle32_v_f32m8_m (mask, in, 4);
    vbool4_t m3 = __riscv_vmflt_vf_f32m8_b4 (v, x, 4);
    vbool4_t m4 = __riscv_vmflt_vf_f32m8_b4_mu (m3, m3, v2, x, 4);
    __riscv_vsm_v_b4 (out, m4, 4);
}

void f8 (void * in, void *out, float x)
{
    vbool4_t mask = *(vbool4_t*)in;
    asm volatile ("":::"memory");
    vfloat32m8_t v = __riscv_vle32_v_f32m8 (in, 4);
    vfloat32m8_t v2 = __riscv_vle32_v_f32m8_m (mask, in, 4);
    vbool4_t m3 = __riscv_vmflt_vf_f32m8_b4 (v, x, 4);
    vbool4_t m4 = __riscv_vmflt_vf_f32m8_b4_m (m3, v2, x, 4);
    __riscv_vsm_v_b4 (out, m3, 4);
    __riscv_vsm_v_b4 (out, m4, 4);
}

void f9 (void * in, void *out, float x)
{
    vbool4_t mask = *(vbool4_t*)in;
    asm volatile ("":::"memory");
    vfloat32m8_t v = __riscv_vle32_v_f32m8 (in, 4);
    vfloat32m8_t v2 = __riscv_vle32_v_f32m8_m (mask, in, 4);
    vbool4_t m3 = __riscv_vmflt_vf_f32m8_b4 (v, x, 4);
    vbool4_t m4 = __riscv_vmflt_vf_f32m8_b4_m (m3, v2, x, 4);
    __riscv_vsm_v_b4 (out, m4, 4);
    vbool4_t m5 = __riscv_vmflt_vf_f32m8_b4_mu (m3, m4, v2, x, 4);
    __riscv_vsm_v_b4 (out, m3, 4);
    __riscv_vsm_v_b4 (out, m5, 4);
}

void f10 (void * in, void *out, float x)
{
    vbool4_t mask = *(vbool4_t*)in;
    asm volatile ("":::"memory");
    vfloat32m8_t v = __riscv_vle32_v_f32m8 (in, 4);
    vfloat32m8_t v2 = __riscv_vle32_v_f32m8_m (mask, in, 4);
    vbool4_t m3 = __riscv_vmflt_vf_f32m8_b4 (v, x, 4);
    vbool4_t m4 = __riscv_vmflt_vf_f32m8_b4_mu (m3, m3, v2, x, 4);
    __riscv_vsm_v_b4 (out, m4, 4);
}

void f11 (void * in, void *out, float x)
{
    vbool4_t mask = *(vbool4_t*)in;
    asm volatile ("":::"memory");
    vfloat32m8_t v = __riscv_vle32_v_f32m8 (in, 4);
    vfloat32m8_t v2 = __riscv_vle32_v_f32m8_m (mask, in, 4);
    vbool4_t m3 = __riscv_vmflt_vf_f32m8_b4 (v, x, 4);
    vbool4_t m4 = __riscv_vmflt_vf_f32m8_b4_m (m3, v2, x, 4);
    __riscv_vsm_v_b4 (out, m3, 4);
    __riscv_vsm_v_b4 (out, m4, 4);
}

void f12 (void* base1,void* base2,void* out,int n, float x)
{
  vbool4_t mask = *(vbool4_t*)base1;
  vfloat32m8_t v = __riscv_vle32_v_f32m8 (base1, 4);
  vfloat32m8_t v2 = __riscv_vle32_v_f32m8_m (mask, base1, 4);
  mask = __riscv_vmflt_vf_f32m8_b4 (v, x, 4);
  for (int i = 0; i < n; i++){
    vfloat32m8_t v3 = __riscv_vle32_v_f32m8 (base1 + i, 4);
    vfloat32m8_t v4 = __riscv_vle32_v_f32m8_m (mask, base1 + i * 2, 4);
    mask = __riscv_vmflt_vf_f32m8_b4_m (mask, v3, x,32);
    mask = __riscv_vmflt_vf_f32m8_b4_mu (mask, mask, v4, x, 32);
  }
  __riscv_vsm_v_b4 (out, mask, 32);
}

void f13 (void* base1,void* base2,void* out,int n, float x)
{
  vbool32_t mask = *(vbool32_t*)base1;
  vfloat32m1_t v = __riscv_vle32_v_f32m1 (base1, 4);
  vfloat32m1_t v2 = __riscv_vle32_v_f32m1_m (mask, base1, 4);
  mask = __riscv_vmflt_vf_f32m1_b32 (v, x, 4);
  for (int i = 0; i < n; i++){
    vfloat32m1_t v3 = __riscv_vle32_v_f32m1 (base1 + i, 4);
    vfloat32m1_t v4 = __riscv_vle32_v_f32m1_m (mask, base1 + i * 2, 4);
    mask = __riscv_vmflt_vf_f32m1_b32_m (mask, v3, x,32);
    mask = __riscv_vmflt_vf_f32m1_b32_mu (mask, mask, v4, x, 32);
  }
  __riscv_vsm_v_b32 (out, mask, 32);
}

/* { dg-final { scan-assembler-not {vmv} } } */
/* { dg-final { scan-assembler-not {csrr} } } */

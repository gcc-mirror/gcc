/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3" } */

#include "riscv_vector.h"

void f0 (void *base,void *out,size_t vl)
{
    vfloat64m1_t src = __riscv_vle64_v_f64m1 (base, vl);
    vfloat32mf2_t v = __riscv_vfncvt_f_f_w_f32mf2(src,vl);
    __riscv_vse32_v_f32mf2 (out,v,vl);
}

void f1 (void *base,void *out,size_t vl)
{
    vfloat64m1_t src = __riscv_vle64_v_f64m1 (base, vl);
    vfloat32mf2_t src2 = __riscv_vle32_v_f32mf2 ((void *)(base + 100), vl);
    vfloat32mf2_t v = __riscv_vfncvt_f_f_w_f32mf2_tu(src2,src,vl);
    __riscv_vse32_v_f32mf2 (out,v,vl);
}

void f2 (void *base,void *out,size_t vl)
{
    vfloat64m1_t src = __riscv_vle64_v_f64m1 (base, vl);
    vfloat32mf2_t v = __riscv_vfncvt_f_f_w_f32mf2(src,vl);
    vfloat64m1_t v2 = __riscv_vfadd_vv_f64m1 (src, src,vl);
    __riscv_vse32_v_f32mf2 (out,v,vl);
    __riscv_vse64_v_f64m1 ((void *)out,v2,vl);
}

void f3 (void *base,void *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vfloat64m1_t src = __riscv_vle64_v_f64m1 (base + 100*i, vl);
      vfloat32mf2_t v = __riscv_vfncvt_f_f_w_f32mf2(src,vl);
      vfloat64m1_t v2 = __riscv_vfadd_vv_f64m1 (src, src,vl);
      __riscv_vse32_v_f32mf2 (out + 100*i,v,vl);
      __riscv_vse64_v_f64m1 ((void *)(out + 200*i),v2,vl);
    }
}

void f4 (void *base,void *out,size_t vl)
{
    vfloat64m1_t src = __riscv_vle64_v_f64m1 (base, vl);
    vfloat32mf2_t v = __riscv_vfncvt_f_f_w_f32mf2(src,vl);
    v = __riscv_vfncvt_f_f_w_f32mf2_tu(v,src,vl);
    v = __riscv_vfncvt_f_f_w_f32mf2_tu(v,src,vl);
    vfloat64m1_t v2 = __riscv_vfadd_vv_f64m1 (src, src,vl);
    __riscv_vse32_v_f32mf2 (out,v,vl);
    __riscv_vse64_v_f64m1 ((void *)out,v2,vl);
}

void f5 (void *base,void *base2,void *out,size_t vl, int n)
{
    vfloat64m1_t src = __riscv_vle64_v_f64m1 (base + 100, vl);
    for (int i = 0; i < n; i++){
      vbool64_t m = __riscv_vlm_v_b64 (base + i, vl);
      vfloat32mf2_t v = __riscv_vfncvt_f_f_w_f32mf2_m(m,src,vl);
      v = __riscv_vfncvt_f_f_w_f32mf2_tu(v,src,vl);
      v = __riscv_vle32_v_f32mf2_tu (v, base2, vl);
      __riscv_vse32_v_f32mf2 (out + 100*i,v,vl);
    }
}

void f6 (void *base,void *out,size_t vl)
{
    vfloat64m2_t src = __riscv_vle64_v_f64m2 (base, vl);
    vfloat32m1_t v = __riscv_vfncvt_f_f_w_f32m1(src,vl);
    __riscv_vse32_v_f32m1 (out,v,vl);
}

void f7 (void *base,void *out,size_t vl)
{
    vfloat64m2_t src = __riscv_vle64_v_f64m2 (base, vl);
    vfloat32m1_t src2 = __riscv_vle32_v_f32m1 ((void *)(base + 100), vl);
    vfloat32m1_t v = __riscv_vfncvt_f_f_w_f32m1_tu(src2,src,vl);
    __riscv_vse32_v_f32m1 (out,v,vl);
}

void f8 (void *base,void *out,size_t vl)
{
    vfloat64m2_t src = __riscv_vle64_v_f64m2 (base, vl);
    vfloat32m1_t v = __riscv_vfncvt_f_f_w_f32m1(src,vl);
    vfloat64m2_t v2 = __riscv_vfadd_vv_f64m2 (src, src,vl);
    __riscv_vse32_v_f32m1 (out,v,vl);
    __riscv_vse64_v_f64m2 ((void *)out,v2,vl);
}

void f9 (void *base,void *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vfloat64m2_t src = __riscv_vle64_v_f64m2 (base + 100*i, vl);
      vfloat32m1_t v = __riscv_vfncvt_f_f_w_f32m1(src,vl);
      vfloat64m2_t v2 = __riscv_vfadd_vv_f64m2 (src, src,vl);
      __riscv_vse32_v_f32m1 (out + 100*i,v,vl);
      __riscv_vse64_v_f64m2 ((void *)(out + 200*i),v2,vl);
    }
}

void f10 (void *base,void *out,size_t vl)
{
    vfloat64m2_t src = __riscv_vle64_v_f64m2 (base, vl);
    vfloat32m1_t v = __riscv_vfncvt_f_f_w_f32m1(src,vl);
    v = __riscv_vfncvt_f_f_w_f32m1_tu(v,src,vl);
    v = __riscv_vfncvt_f_f_w_f32m1_tu(v,src,vl);
    vfloat64m2_t v2 = __riscv_vfadd_vv_f64m2 (src, src,vl);
    __riscv_vse32_v_f32m1 (out,v,vl);
    __riscv_vse64_v_f64m2 ((void *)out,v2,vl);
}

void f11 (void *base,void *base2,void *out,size_t vl, int n)
{
    vfloat64m2_t src = __riscv_vle64_v_f64m2 (base + 100, vl);
    for (int i = 0; i < n; i++){
      vbool32_t m = __riscv_vlm_v_b32 (base + i, vl);
      vfloat32m1_t v = __riscv_vfncvt_f_f_w_f32m1_m(m,src,vl);
      v = __riscv_vfncvt_f_f_w_f32m1_tu(v,src,vl);
      v = __riscv_vle32_v_f32m1_tu (v, base2, vl);
      __riscv_vse32_v_f32m1 (out + 100*i,v,vl);
    }
}

void f12 (void *base,void *out,size_t vl, int n)
{
    vfloat32mf2_t v = __riscv_vle32_v_f32mf2 ((void *)(base + 1000), vl);
    for (int i = 0; i < n; i++){
      vfloat64m1_t src = __riscv_vle64_v_f64m1 (base + 100*i, vl);
      v = __riscv_vfncvt_f_f_w_f32mf2_tu(v,src,vl);
      v = __riscv_vfncvt_f_f_w_f32mf2_tu(v,src,vl);
      v = __riscv_vfncvt_f_f_w_f32mf2_tu(v,src,vl);
      v = __riscv_vfncvt_f_f_w_f32mf2_tu(v,src,vl);
      v = __riscv_vfncvt_f_f_w_f32mf2_tu(v,src,vl);
      v = __riscv_vfncvt_f_f_w_f32mf2_tu(v,src,vl);
      __riscv_vse32_v_f32mf2 (out + 100*i,v,vl);
    }
}

void f13 (void *base,void *out,size_t vl, int n)
{
    vfloat32m1_t v = __riscv_vle32_v_f32m1 ((void *)(base + 1000), vl);
    for (int i = 0; i < n; i++){
      vfloat64m2_t src = __riscv_vle64_v_f64m2 (base + 100*i, vl);
      v = __riscv_vfncvt_f_f_w_f32m1_tu(v,src,vl);
      v = __riscv_vfncvt_f_f_w_f32m1_tu(v,src,vl);
      v = __riscv_vfncvt_f_f_w_f32m1_tu(v,src,vl);
      v = __riscv_vfncvt_f_f_w_f32m1_tu(v,src,vl);
      v = __riscv_vfncvt_f_f_w_f32m1_tu(v,src,vl);
      v = __riscv_vfncvt_f_f_w_f32m1_tu(v,src,vl);
      __riscv_vse32_v_f32m1 (out + 100*i,v,vl);
    }
}

void f14 (void *base,void *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vfloat32mf2_t v = __riscv_vle32_v_f32mf2 ((void *)(base + 1000 * i), vl);
      vfloat64m1_t src = __riscv_vle64_v_f64m1 (base + 100*i, vl);
      v = __riscv_vfncvt_f_f_w_f32mf2_tu(v,src,vl);
      v = __riscv_vfncvt_f_f_w_f32mf2_tu(v,src,vl);
      v = __riscv_vfncvt_f_f_w_f32mf2_tu(v,src,vl);
      v = __riscv_vfncvt_f_f_w_f32mf2_tu(v,src,vl);
      v = __riscv_vfncvt_f_f_w_f32mf2_tu(v,src,vl);
      v = __riscv_vfncvt_f_f_w_f32mf2_tu(v,src,vl);
      __riscv_vse32_v_f32mf2 (out + 100*i,v,vl);
    }
}

void f15 (void *base,void *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vfloat32m1_t v = __riscv_vle32_v_f32m1 ((void *)(base + 1000 * i), vl);
      vfloat64m2_t src = __riscv_vle64_v_f64m2 (base + 100*i, vl);
      v = __riscv_vfncvt_f_f_w_f32m1_tu(v,src,vl);
      v = __riscv_vfncvt_f_f_w_f32m1_tu(v,src,vl);
      v = __riscv_vfncvt_f_f_w_f32m1_tu(v,src,vl);
      v = __riscv_vfncvt_f_f_w_f32m1_tu(v,src,vl);
      v = __riscv_vfncvt_f_f_w_f32m1_tu(v,src,vl);
      v = __riscv_vfncvt_f_f_w_f32m1_tu(v,src,vl);
      __riscv_vse32_v_f32m1 (out + 100*i,v,vl);
    }
}

void f16 (void *base,void *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vfloat32mf2_t v = __riscv_vle32_v_f32mf2 ((void *)(base + 1000 * i), vl);
      vfloat64m1_t src1 = __riscv_vle64_v_f64m1 (base + 100*i, vl);
      vfloat64m1_t src2 = __riscv_vle64_v_f64m1 (base + 200*i, vl);
      vfloat64m1_t src3 = __riscv_vle64_v_f64m1 (base + 300*i, vl);
      vfloat64m1_t src4 = __riscv_vle64_v_f64m1 (base + 400*i, vl);
      vfloat64m1_t src5 = __riscv_vle64_v_f64m1 (base + 500*i, vl);
      vfloat64m1_t src6 = __riscv_vle64_v_f64m1 (base + 600*i, vl);
      v = __riscv_vfncvt_f_f_w_f32mf2_tu(v,src1,vl);
      v = __riscv_vfncvt_f_f_w_f32mf2_tu(v,src2,vl);
      v = __riscv_vfncvt_f_f_w_f32mf2_tu(v,src3,vl);
      v = __riscv_vfncvt_f_f_w_f32mf2_tu(v,src4,vl);
      v = __riscv_vfncvt_f_f_w_f32mf2_tu(v,src5,vl);
      v = __riscv_vfncvt_f_f_w_f32mf2_tu(v,src6,vl);
      __riscv_vse32_v_f32mf2 (out + 100*i,v,vl);
    }
}

void f17 (void *base,void *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vfloat32m1_t v = __riscv_vle32_v_f32m1 ((void *)(base + 1000 * i), vl);
      vfloat64m2_t src1 = __riscv_vle64_v_f64m2 (base + 100*i, vl);
      vfloat64m2_t src2 = __riscv_vle64_v_f64m2 (base + 200*i, vl);
      vfloat64m2_t src3 = __riscv_vle64_v_f64m2 (base + 300*i, vl);
      vfloat64m2_t src4 = __riscv_vle64_v_f64m2 (base + 400*i, vl);
      vfloat64m2_t src5 = __riscv_vle64_v_f64m2 (base + 500*i, vl);
      vfloat64m2_t src6 = __riscv_vle64_v_f64m2 (base + 600*i, vl);
      v = __riscv_vfncvt_f_f_w_f32m1_tu(v,src1,vl);
      v = __riscv_vfncvt_f_f_w_f32m1_tu(v,src2,vl);
      v = __riscv_vfncvt_f_f_w_f32m1_tu(v,src3,vl);
      v = __riscv_vfncvt_f_f_w_f32m1_tu(v,src4,vl);
      v = __riscv_vfncvt_f_f_w_f32m1_tu(v,src5,vl);
      v = __riscv_vfncvt_f_f_w_f32m1_tu(v,src6,vl);
      __riscv_vse32_v_f32m1 (out + 100*i,v,vl);
    }
}

void f18 (void *base,void *out,size_t vl)
{
    vfloat64m2_t src = __riscv_vle64_v_f64m2 (base, vl);
    /* Only allow load v30,v31.  */
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29");

    vfloat32m1_t v = __riscv_vfncvt_f_f_w_f32m1(src,vl);
    /* Only allow vncvt SRC == DEST v30.  */
    asm volatile("#" ::                                                        
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", 
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17",     
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",     
		   "v26", "v27", "v28", "v29", "v31");

    __riscv_vse32_v_f32m1 (out,v,vl);
}

void f19 (void *base,void *out,size_t vl)
{
    vfloat64m1_t src = __riscv_vle64_v_f64m1 (base, vl);
    /* Only allow load v31.  */
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v30");

    vfloat32mf2_t v = __riscv_vfncvt_f_f_w_f32mf2(src,vl);
    /* Only allow vncvt SRC == DEST v31.  */
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v30");

    __riscv_vse32_v_f32mf2 (out,v,vl);
}

void f20 (void *base,void *out,size_t vl)
{
    vfloat64m2_t src = __riscv_vle64_v_f64m2 (base, vl);
    /* Only allow load v30,v31.  */
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29");

    vfloat32m1_t v = __riscv_vfncvt_f_f_w_f32m1(src,vl);
    /* Only allow v29.  */
    asm volatile("#" ::                                                        
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", 
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17",     
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",     
		   "v26", "v27", "v28", "v30", "v31");
    v = __riscv_vfadd_vv_f32m1 (v,v,vl);
    /* Only allow v29.  */
    asm volatile("#" ::                                                        
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", 
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17",     
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",     
		   "v26", "v27", "v28", "v30", "v31");

    __riscv_vse32_v_f32m1 (out,v,vl);
}

/* { dg-final { scan-assembler-not {vmv} } } */
/* { dg-final { scan-assembler-not {csrr} } } */

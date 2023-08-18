/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-sched-pressure" } */

#include "riscv_vector.h"

void f0 (int16_t *base,int8_t *out,size_t vl)
{
    vint16mf4_t src = __riscv_vle16_v_i16mf4 (base, vl);
    vint8mf8_t v = __riscv_vncvt_x_x_w_i8mf8(src,vl);
    __riscv_vse8_v_i8mf8 (out,v,vl);
}

void f1 (int16_t *base,int8_t *out,size_t vl)
{
    vint16mf4_t src = __riscv_vle16_v_i16mf4 (base, vl);
    vint8mf8_t src2 = __riscv_vle8_v_i8mf8 ((int8_t *)(base + 100), vl);
    vint8mf8_t v = __riscv_vncvt_x_x_w_i8mf8_tu(src2,src,vl);
    __riscv_vse8_v_i8mf8 (out,v,vl);
}

void f2 (int16_t *base,int8_t *out,size_t vl)
{
    vint16mf4_t src = __riscv_vle16_v_i16mf4 (base, vl);
    vint8mf8_t v = __riscv_vncvt_x_x_w_i8mf8(src,vl);
    vint16mf4_t v2 = __riscv_vadd_vv_i16mf4 (src, src,vl);
    __riscv_vse8_v_i8mf8 (out,v,vl);
    __riscv_vse16_v_i16mf4 ((int16_t *)out,v2,vl);
}

void f3 (int16_t *base,int8_t *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vint16mf4_t src = __riscv_vle16_v_i16mf4 (base + 100*i, vl);
      vint8mf8_t v = __riscv_vncvt_x_x_w_i8mf8(src,vl);
      vint16mf4_t v2 = __riscv_vadd_vv_i16mf4 (src, src,vl);
      __riscv_vse8_v_i8mf8 (out + 100*i,v,vl);
      __riscv_vse16_v_i16mf4 ((int16_t *)(out + 200*i),v2,vl);
    }
}

void f4 (int16_t *base,int8_t *out,size_t vl)
{
    vint16mf4_t src = __riscv_vle16_v_i16mf4 (base, vl);
    vint8mf8_t v = __riscv_vncvt_x_x_w_i8mf8(src,vl);
    v = __riscv_vncvt_x_x_w_i8mf8_tu(v,src,vl);
    v = __riscv_vncvt_x_x_w_i8mf8_tu(v,src,vl);
    vint16mf4_t v2 = __riscv_vadd_vv_i16mf4 (src, src,vl);
    __riscv_vse8_v_i8mf8 (out,v,vl);
    __riscv_vse16_v_i16mf4 ((int16_t *)out,v2,vl);
}

void f5 (void *base,void *base2,void *out,size_t vl, int n)
{
    vint16mf4_t src = __riscv_vle16_v_i16mf4 (base + 100, vl);
    for (int i = 0; i < n; i++){
      vbool64_t m = __riscv_vlm_v_b64 (base + i, vl);
      vint8mf8_t v = __riscv_vncvt_x_x_w_i8mf8_m(m,src,vl);
      v = __riscv_vncvt_x_x_w_i8mf8_tu(v,src,vl);
      v = __riscv_vle8_v_i8mf8_tu (v, base2, vl);
      __riscv_vse8_v_i8mf8 (out + 100*i,v,vl);
    }
}

void f6 (int16_t *base,int8_t *out,size_t vl)
{
    vint16m2_t src = __riscv_vle16_v_i16m2 (base, vl);
    vint8m1_t v = __riscv_vncvt_x_x_w_i8m1(src,vl);
    __riscv_vse8_v_i8m1 (out,v,vl);
}

void f7 (int16_t *base,int8_t *out,size_t vl)
{
    vint16m2_t src = __riscv_vle16_v_i16m2 (base, vl);
    vint8m1_t src2 = __riscv_vle8_v_i8m1 ((int8_t *)(base + 100), vl);
    vint8m1_t v = __riscv_vncvt_x_x_w_i8m1_tu(src2,src,vl);
    __riscv_vse8_v_i8m1 (out,v,vl);
}

void f8 (int16_t *base,int8_t *out,size_t vl)
{
    vint16m2_t src = __riscv_vle16_v_i16m2 (base, vl);
    vint8m1_t v = __riscv_vncvt_x_x_w_i8m1(src,vl);
    vint16m2_t v2 = __riscv_vadd_vv_i16m2 (src, src,vl);
    __riscv_vse8_v_i8m1 (out,v,vl);
    __riscv_vse16_v_i16m2 ((int16_t *)out,v2,vl);
}

void f9 (int16_t *base,int8_t *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vint16m2_t src = __riscv_vle16_v_i16m2 (base + 100*i, vl);
      vint8m1_t v = __riscv_vncvt_x_x_w_i8m1(src,vl);
      vint16m2_t v2 = __riscv_vadd_vv_i16m2 (src, src,vl);
      __riscv_vse8_v_i8m1 (out + 100*i,v,vl);
      __riscv_vse16_v_i16m2 ((int16_t *)(out + 200*i),v2,vl);
    }
}

void f10 (int16_t *base,int8_t *out,size_t vl)
{
    vint16m2_t src = __riscv_vle16_v_i16m2 (base, vl);
    vint8m1_t v = __riscv_vncvt_x_x_w_i8m1(src,vl);
    v = __riscv_vncvt_x_x_w_i8m1_tu(v,src,vl);
    v = __riscv_vncvt_x_x_w_i8m1_tu(v,src,vl);
    vint16m2_t v2 = __riscv_vadd_vv_i16m2 (src, src,vl);
    __riscv_vse8_v_i8m1 (out,v,vl);
    __riscv_vse16_v_i16m2 ((int16_t *)out,v2,vl);
}

void f11 (void *base,void *base2,void *out,size_t vl, int n)
{
    vint16m2_t src = __riscv_vle16_v_i16m2 (base + 100, vl);
    for (int i = 0; i < n; i++){
      vbool8_t m = __riscv_vlm_v_b8 (base + i, vl);
      vint8m1_t v = __riscv_vncvt_x_x_w_i8m1_m(m,src,vl);
      v = __riscv_vncvt_x_x_w_i8m1_tu(v,src,vl);
      v = __riscv_vle8_v_i8m1_tu (v, base2, vl);
      __riscv_vse8_v_i8m1 (out + 100*i,v,vl);
    }
}

void f12 (int16_t *base,int8_t *out,size_t vl, int n)
{
    vint8mf8_t v = __riscv_vle8_v_i8mf8 ((int8_t *)(base + 1000), vl);
    for (int i = 0; i < n; i++){
      vint16mf4_t src = __riscv_vle16_v_i16mf4 (base + 100*i, vl);
      v = __riscv_vncvt_x_x_w_i8mf8_tu(v,src,vl);
      v = __riscv_vncvt_x_x_w_i8mf8_tu(v,src,vl);
      v = __riscv_vncvt_x_x_w_i8mf8_tu(v,src,vl);
      v = __riscv_vncvt_x_x_w_i8mf8_tu(v,src,vl);
      v = __riscv_vncvt_x_x_w_i8mf8_tu(v,src,vl);
      v = __riscv_vncvt_x_x_w_i8mf8_tu(v,src,vl);
      __riscv_vse8_v_i8mf8 (out + 100*i,v,vl);
    }
}

void f13 (int16_t *base,int8_t *out,size_t vl, int n)
{
    vint8m1_t v = __riscv_vle8_v_i8m1 ((int8_t *)(base + 1000), vl);
    for (int i = 0; i < n; i++){
      vint16m2_t src = __riscv_vle16_v_i16m2 (base + 100*i, vl);
      v = __riscv_vncvt_x_x_w_i8m1_tu(v,src,vl);
      v = __riscv_vncvt_x_x_w_i8m1_tu(v,src,vl);
      v = __riscv_vncvt_x_x_w_i8m1_tu(v,src,vl);
      v = __riscv_vncvt_x_x_w_i8m1_tu(v,src,vl);
      v = __riscv_vncvt_x_x_w_i8m1_tu(v,src,vl);
      v = __riscv_vncvt_x_x_w_i8m1_tu(v,src,vl);
      __riscv_vse8_v_i8m1 (out + 100*i,v,vl);
    }
}

void f14 (int16_t *base,int8_t *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vint8mf8_t v = __riscv_vle8_v_i8mf8 ((int8_t *)(base + 1000 * i), vl);
      vint16mf4_t src = __riscv_vle16_v_i16mf4 (base + 100*i, vl);
      v = __riscv_vncvt_x_x_w_i8mf8_tu(v,src,vl);
      v = __riscv_vncvt_x_x_w_i8mf8_tu(v,src,vl);
      v = __riscv_vncvt_x_x_w_i8mf8_tu(v,src,vl);
      v = __riscv_vncvt_x_x_w_i8mf8_tu(v,src,vl);
      v = __riscv_vncvt_x_x_w_i8mf8_tu(v,src,vl);
      v = __riscv_vncvt_x_x_w_i8mf8_tu(v,src,vl);
      __riscv_vse8_v_i8mf8 (out + 100*i,v,vl);
    }
}

void f15 (int16_t *base,int8_t *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vint8m1_t v = __riscv_vle8_v_i8m1 ((int8_t *)(base + 1000 * i), vl);
      vint16m2_t src = __riscv_vle16_v_i16m2 (base + 100*i, vl);
      v = __riscv_vncvt_x_x_w_i8m1_tu(v,src,vl);
      v = __riscv_vncvt_x_x_w_i8m1_tu(v,src,vl);
      v = __riscv_vncvt_x_x_w_i8m1_tu(v,src,vl);
      v = __riscv_vncvt_x_x_w_i8m1_tu(v,src,vl);
      v = __riscv_vncvt_x_x_w_i8m1_tu(v,src,vl);
      v = __riscv_vncvt_x_x_w_i8m1_tu(v,src,vl);
      __riscv_vse8_v_i8m1 (out + 100*i,v,vl);
    }
}

void f16 (int16_t *base,int8_t *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vint8mf8_t v = __riscv_vle8_v_i8mf8 ((int8_t *)(base + 1000 * i), vl);
      vint16mf4_t src1 = __riscv_vle16_v_i16mf4 (base + 100*i, vl);
      vint16mf4_t src2 = __riscv_vle16_v_i16mf4 (base + 200*i, vl);
      vint16mf4_t src3 = __riscv_vle16_v_i16mf4 (base + 300*i, vl);
      vint16mf4_t src4 = __riscv_vle16_v_i16mf4 (base + 400*i, vl);
      vint16mf4_t src5 = __riscv_vle16_v_i16mf4 (base + 500*i, vl);
      vint16mf4_t src6 = __riscv_vle16_v_i16mf4 (base + 600*i, vl);
      v = __riscv_vncvt_x_x_w_i8mf8_tu(v,src1,vl);
      v = __riscv_vncvt_x_x_w_i8mf8_tu(v,src2,vl);
      v = __riscv_vncvt_x_x_w_i8mf8_tu(v,src3,vl);
      v = __riscv_vncvt_x_x_w_i8mf8_tu(v,src4,vl);
      v = __riscv_vncvt_x_x_w_i8mf8_tu(v,src5,vl);
      v = __riscv_vncvt_x_x_w_i8mf8_tu(v,src6,vl);
      __riscv_vse8_v_i8mf8 (out + 100*i,v,vl);
    }
}

void f17 (int16_t *base,int8_t *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vint8m1_t v = __riscv_vle8_v_i8m1 ((int8_t *)(base + 1000 * i), vl);
      vint16m2_t src1 = __riscv_vle16_v_i16m2 (base + 100*i, vl);
      vint16m2_t src2 = __riscv_vle16_v_i16m2 (base + 200*i, vl);
      vint16m2_t src3 = __riscv_vle16_v_i16m2 (base + 300*i, vl);
      vint16m2_t src4 = __riscv_vle16_v_i16m2 (base + 400*i, vl);
      vint16m2_t src5 = __riscv_vle16_v_i16m2 (base + 500*i, vl);
      vint16m2_t src6 = __riscv_vle16_v_i16m2 (base + 600*i, vl);
      v = __riscv_vncvt_x_x_w_i8m1_tu(v,src1,vl);
      v = __riscv_vncvt_x_x_w_i8m1_tu(v,src2,vl);
      v = __riscv_vncvt_x_x_w_i8m1_tu(v,src3,vl);
      v = __riscv_vncvt_x_x_w_i8m1_tu(v,src4,vl);
      v = __riscv_vncvt_x_x_w_i8m1_tu(v,src5,vl);
      v = __riscv_vncvt_x_x_w_i8m1_tu(v,src6,vl);
      __riscv_vse8_v_i8m1 (out + 100*i,v,vl);
    }
}

void f18 (void *base,void *base2,void *out,size_t vl, int n)
{
    vint32mf2_t src = __riscv_vle32_v_i32mf2 (base + 100, vl);
    for (int i = 0; i < n; i++){
      vbool64_t m = __riscv_vlm_v_b64 (base + i, vl);
      vint16mf4_t v = __riscv_vncvt_x_x_w_i16mf4_m(m,src,vl);
      vint16mf4_t v2 = __riscv_vle16_v_i16mf4_tu (v, base2 + i, vl);
      vint8mf8_t v3 = __riscv_vncvt_x_x_w_i8mf8_m(m,v2,vl);
      __riscv_vse8_v_i8mf8 (out + 100*i,v3,vl);
    }
}

void f19 (void *base,void *base2,void *out,size_t vl, int n)
{
    vint32m4_t src = __riscv_vle32_v_i32m4 (base + 100, vl);
    for (int i = 0; i < n; i++){
      vbool8_t m = __riscv_vlm_v_b8 (base + i, vl);
      vint16m2_t v = __riscv_vncvt_x_x_w_i16m2_m(m,src,vl);
      vint16m2_t v2 = __riscv_vle16_v_i16m2_tu (v, base2 + i, vl);
      vint8m1_t v3 = __riscv_vncvt_x_x_w_i8m1_m(m,v2,vl);
      vint8m1_t v4 = __riscv_vncvt_x_x_w_i8m1_tumu(m,v3,v2,vl);
      __riscv_vse8_v_i8m1 (out + 100*i,v3,vl);
      __riscv_vse8_v_i8m1 (out + 222*i,v4,vl);
    }
}

void f20 (int16_t *base,int8_t *out,size_t vl)
{
    vint16m2_t src = __riscv_vle16_v_i16m2 (base, vl);
    /* Only allow load v30,v31.  */
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29");

    vint8m1_t v = __riscv_vncvt_x_x_w_i8m1(src,vl);
    /* Only allow vncvt SRC == DEST v30.  */
    asm volatile("#" ::                                                        
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", 
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17",     
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",     
		   "v26", "v27", "v28", "v29", "v31");

    __riscv_vse8_v_i8m1 (out,v,vl);
}

void f21 (int16_t *base,int8_t *out,size_t vl)
{
    vint16m1_t src = __riscv_vle16_v_i16m1 (base, vl);
    /* Only allow load v31.  */
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v30");

    vint8mf2_t v = __riscv_vncvt_x_x_w_i8mf2(src,vl);
    /* Only allow vncvt SRC == DEST v31.  */
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v30");

    __riscv_vse8_v_i8mf2 (out,v,vl);
}

void f22 (int16_t *base,int8_t *out,size_t vl)
{
    vint16m2_t src = __riscv_vle16_v_i16m2 (base, vl);
    /* Only allow load v30,v31.  */
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29");

    vint8m1_t v = __riscv_vncvt_x_x_w_i8m1(src,vl);
    /* Only allow v29.  */
    asm volatile("#" ::                                                        
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", 
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17",     
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",     
		   "v26", "v27", "v28", "v30", "v31");
    v = __riscv_vadd_vv_i8m1 (v,v,vl);
    /* Only allow v29.  */
    asm volatile("#" ::                                                        
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", 
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17",     
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",     
		   "v26", "v27", "v28", "v30", "v31");

    __riscv_vse8_v_i8m1 (out,v,vl);
}

/* { dg-final { scan-assembler-not {vmv} } } */
/* { dg-final { scan-assembler-not {csrr} } } */

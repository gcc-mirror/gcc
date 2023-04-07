/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3" } */

#include "riscv_vector.h"

void f0 (int16_t *base,int8_t *out,size_t vl, size_t shift)
{
    vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base, vl);
    vuint8mf8_t v = __riscv_vnclipu_wx_u8mf8(src,31,vl);
    __riscv_vse8_v_u8mf8 (out,v,vl);
}

void f1 (int16_t *base,int8_t *out,size_t vl, size_t shift)
{
    vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base, vl);
    vuint8mf8_t src2 = __riscv_vle8_v_u8mf8 ((int8_t *)(base + 100), vl);
    vuint8mf8_t v = __riscv_vnclipu_wx_u8mf8_tu(src2,src,31,vl);
    __riscv_vse8_v_u8mf8 (out,v,vl);
}

void f2 (int16_t *base,int8_t *out,size_t vl, size_t shift)
{
    vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base, vl);
    vuint8mf8_t v = __riscv_vnclipu_wx_u8mf8(src,31,vl);
    vuint16mf4_t v2 = __riscv_vadd_vv_u16mf4 (src, src,vl);
    __riscv_vse8_v_u8mf8 (out,v,vl);
    __riscv_vse16_v_u16mf4 ((int16_t *)out,v2,vl);
}

void f3 (int16_t *base,int8_t *out,size_t vl, int n, size_t shift)
{
    for (int i = 0; i < n; i++){
      vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base + 100*i, vl);
      vuint8mf8_t v = __riscv_vnclipu_wx_u8mf8(src,31,vl);
      vuint16mf4_t v2 = __riscv_vadd_vv_u16mf4 (src, src,vl);
      __riscv_vse8_v_u8mf8 (out + 100*i,v,vl);
      __riscv_vse16_v_u16mf4 ((int16_t *)(out + 200*i),v2,vl);
    }
}

void f4 (int16_t *base,int8_t *out,size_t vl, size_t shift)
{
    vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base, vl);
    vuint8mf8_t v = __riscv_vnclipu_wx_u8mf8(src,31,vl);
    v = __riscv_vnclipu_wx_u8mf8_tu(v,src,31,vl);
    v = __riscv_vnclipu_wx_u8mf8_tu(v,src,31,vl);
    vuint16mf4_t v2 = __riscv_vadd_vv_u16mf4 (src, src,vl);
    __riscv_vse8_v_u8mf8 (out,v,vl);
    __riscv_vse16_v_u16mf4 ((int16_t *)out,v2,vl);
}

void f5 (void *base,void *base2,void *out,size_t vl, int n, size_t shift)
{
    vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base + 100, vl);
    for (int i = 0; i < n; i++){
      vbool64_t m = __riscv_vlm_v_b64 (base + i, vl);
      vuint8mf8_t v = __riscv_vnclipu_wx_u8mf8_m(m,src,31,vl);
      v = __riscv_vnclipu_wx_u8mf8_tu(v,src,31,vl);
      v = __riscv_vle8_v_u8mf8_tu (v, base2, vl);
      __riscv_vse8_v_u8mf8 (out + 100*i,v,vl);
    }
}

void f6 (int16_t *base,int8_t *out,size_t vl, size_t shift)
{
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base, vl);
    vuint8m1_t v = __riscv_vnclipu_wx_u8m1(src,31,vl);
    __riscv_vse8_v_u8m1 (out,v,vl);
}

void f7 (int16_t *base,int8_t *out,size_t vl, size_t shift)
{
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base, vl);
    vuint8m1_t src2 = __riscv_vle8_v_u8m1 ((int8_t *)(base + 100), vl);
    vuint8m1_t v = __riscv_vnclipu_wx_u8m1_tu(src2,src,31,vl);
    __riscv_vse8_v_u8m1 (out,v,vl);
}

void f8 (int16_t *base,int8_t *out,size_t vl, size_t shift)
{
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base, vl);
    vuint8m1_t v = __riscv_vnclipu_wx_u8m1(src,31,vl);
    vuint16m2_t v2 = __riscv_vadd_vv_u16m2 (src, src,vl);
    __riscv_vse8_v_u8m1 (out,v,vl);
    __riscv_vse16_v_u16m2 ((int16_t *)out,v2,vl);
}

void f9 (int16_t *base,int8_t *out,size_t vl, int n, size_t shift)
{
    for (int i = 0; i < n; i++){
      vuint16m2_t src = __riscv_vle16_v_u16m2 (base + 100*i, vl);
      vuint8m1_t v = __riscv_vnclipu_wx_u8m1(src,31,vl);
      vuint16m2_t v2 = __riscv_vadd_vv_u16m2 (src, src,vl);
      __riscv_vse8_v_u8m1 (out + 100*i,v,vl);
      __riscv_vse16_v_u16m2 ((int16_t *)(out + 200*i),v2,vl);
    }
}

void f10 (int16_t *base,int8_t *out,size_t vl, size_t shift)
{
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base, vl);
    vuint8m1_t v = __riscv_vnclipu_wx_u8m1(src,31,vl);
    v = __riscv_vnclipu_wx_u8m1_tu(v,src,31,vl);
    v = __riscv_vnclipu_wx_u8m1_tu(v,src,31,vl);
    vuint16m2_t v2 = __riscv_vadd_vv_u16m2 (src, src,vl);
    __riscv_vse8_v_u8m1 (out,v,vl);
    __riscv_vse16_v_u16m2 ((int16_t *)out,v2,vl);
}

void f11 (void *base,void *base2,void *out,size_t vl, int n, size_t shift)
{
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base + 100, vl);
    for (int i = 0; i < n; i++){
      vbool8_t m = __riscv_vlm_v_b8 (base + i, vl);
      vuint8m1_t v = __riscv_vnclipu_wx_u8m1_m(m,src,31,vl);
      v = __riscv_vnclipu_wx_u8m1_tu(v,src,31,vl);
      v = __riscv_vle8_v_u8m1_tu (v, base2, vl);
      __riscv_vse8_v_u8m1 (out + 100*i,v,vl);
    }
}

void f12 (int16_t *base,int8_t *out,size_t vl, int n, size_t shift)
{
    vuint8mf8_t v = __riscv_vle8_v_u8mf8 ((int8_t *)(base + 1000), vl);
    for (int i = 0; i < n; i++){
      vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base + 100*i, vl);
      v = __riscv_vnclipu_wx_u8mf8_tu(v,src,31,vl);
      v = __riscv_vnclipu_wx_u8mf8_tu(v,src,31,vl);
      v = __riscv_vnclipu_wx_u8mf8_tu(v,src,31,vl);
      v = __riscv_vnclipu_wx_u8mf8_tu(v,src,31,vl);
      v = __riscv_vnclipu_wx_u8mf8_tu(v,src,31,vl);
      v = __riscv_vnclipu_wx_u8mf8_tu(v,src,31,vl);
      __riscv_vse8_v_u8mf8 (out + 100*i,v,vl);
    }
}

void f13 (int16_t *base,int8_t *out,size_t vl, int n, size_t shift)
{
    vuint8m1_t v = __riscv_vle8_v_u8m1 ((int8_t *)(base + 1000), vl);
    for (int i = 0; i < n; i++){
      vuint16m2_t src = __riscv_vle16_v_u16m2 (base + 100*i, vl);
      v = __riscv_vnclipu_wx_u8m1_tu(v,src,31,vl);
      v = __riscv_vnclipu_wx_u8m1_tu(v,src,31,vl);
      v = __riscv_vnclipu_wx_u8m1_tu(v,src,31,vl);
      v = __riscv_vnclipu_wx_u8m1_tu(v,src,31,vl);
      v = __riscv_vnclipu_wx_u8m1_tu(v,src,31,vl);
      v = __riscv_vnclipu_wx_u8m1_tu(v,src,31,vl);
      __riscv_vse8_v_u8m1 (out + 100*i,v,vl);
    }
}

void f14 (int16_t *base,int8_t *out,size_t vl, int n, size_t shift)
{
    for (int i = 0; i < n; i++){
      vuint8mf8_t v = __riscv_vle8_v_u8mf8 ((int8_t *)(base + 1000 * i), vl);
      vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base + 100*i, vl);
      v = __riscv_vnclipu_wx_u8mf8_tu(v,src,31,vl);
      v = __riscv_vnclipu_wx_u8mf8_tu(v,src,31,vl);
      v = __riscv_vnclipu_wx_u8mf8_tu(v,src,31,vl);
      v = __riscv_vnclipu_wx_u8mf8_tu(v,src,31,vl);
      v = __riscv_vnclipu_wx_u8mf8_tu(v,src,31,vl);
      v = __riscv_vnclipu_wx_u8mf8_tu(v,src,31,vl);
      __riscv_vse8_v_u8mf8 (out + 100*i,v,vl);
    }
}

void f15 (int16_t *base,int8_t *out,size_t vl, int n, size_t shift)
{
    for (int i = 0; i < n; i++){
      vuint8m1_t v = __riscv_vle8_v_u8m1 ((int8_t *)(base + 1000 * i), vl);
      vuint16m2_t src = __riscv_vle16_v_u16m2 (base + 100*i, vl);
      v = __riscv_vnclipu_wx_u8m1_tu(v,src,31,vl);
      v = __riscv_vnclipu_wx_u8m1_tu(v,src,31,vl);
      v = __riscv_vnclipu_wx_u8m1_tu(v,src,31,vl);
      v = __riscv_vnclipu_wx_u8m1_tu(v,src,31,vl);
      v = __riscv_vnclipu_wx_u8m1_tu(v,src,31,vl);
      v = __riscv_vnclipu_wx_u8m1_tu(v,src,31,vl);
      __riscv_vse8_v_u8m1 (out + 100*i,v,vl);
    }
}

void f16 (int16_t *base,int8_t *out,size_t vl, int n, size_t shift)
{
    for (int i = 0; i < n; i++){
      vuint8mf8_t v = __riscv_vle8_v_u8mf8 ((int8_t *)(base + 1000 * i), vl);
      vuint16mf4_t src1 = __riscv_vle16_v_u16mf4 (base + 100*i, vl);
      vuint16mf4_t src2 = __riscv_vle16_v_u16mf4 (base + 200*i, vl);
      vuint16mf4_t src3 = __riscv_vle16_v_u16mf4 (base + 300*i, vl);
      vuint16mf4_t src4 = __riscv_vle16_v_u16mf4 (base + 400*i, vl);
      vuint16mf4_t src5 = __riscv_vle16_v_u16mf4 (base + 500*i, vl);
      vuint16mf4_t src6 = __riscv_vle16_v_u16mf4 (base + 600*i, vl);
      v = __riscv_vnclipu_wx_u8mf8_tu(v,src1,31,vl);
      v = __riscv_vnclipu_wx_u8mf8_tu(v,src2,31,vl);
      v = __riscv_vnclipu_wx_u8mf8_tu(v,src3,31,vl);
      v = __riscv_vnclipu_wx_u8mf8_tu(v,src4,31,vl);
      v = __riscv_vnclipu_wx_u8mf8_tu(v,src5,31,vl);
      v = __riscv_vnclipu_wx_u8mf8_tu(v,src6,31,vl);
      __riscv_vse8_v_u8mf8 (out + 100*i,v,vl);
    }
}

void f17 (int16_t *base,int8_t *out,size_t vl, int n, size_t shift)
{
    for (int i = 0; i < n; i++){
      vuint8m1_t v = __riscv_vle8_v_u8m1 ((int8_t *)(base + 1000 * i), vl);
      vuint16m2_t src1 = __riscv_vle16_v_u16m2 (base + 100*i, vl);
      vuint16m2_t src2 = __riscv_vle16_v_u16m2 (base + 200*i, vl);
      vuint16m2_t src3 = __riscv_vle16_v_u16m2 (base + 300*i, vl);
      vuint16m2_t src4 = __riscv_vle16_v_u16m2 (base + 400*i, vl);
      vuint16m2_t src5 = __riscv_vle16_v_u16m2 (base + 500*i, vl);
      vuint16m2_t src6 = __riscv_vle16_v_u16m2 (base + 600*i, vl);
      v = __riscv_vnclipu_wx_u8m1_tu(v,src1,31,vl);
      v = __riscv_vnclipu_wx_u8m1_tu(v,src2,31,vl);
      v = __riscv_vnclipu_wx_u8m1_tu(v,src3,31,vl);
      v = __riscv_vnclipu_wx_u8m1_tu(v,src4,31,vl);
      v = __riscv_vnclipu_wx_u8m1_tu(v,src5,31,vl);
      v = __riscv_vnclipu_wx_u8m1_tu(v,src6,31,vl);
      __riscv_vse8_v_u8m1 (out + 100*i,v,vl);
    }
}

void f18 (void *base,void *base2,void *out,size_t vl, int n, size_t shift)
{
    vuint32mf2_t src = __riscv_vle32_v_u32mf2 (base + 100, vl);
    for (int i = 0; i < n; i++){
      vbool64_t m = __riscv_vlm_v_b64 (base + i, vl);
      vuint16mf4_t v = __riscv_vnclipu_wx_u16mf4_m(m,src,31,vl);
      vuint16mf4_t v2 = __riscv_vle16_v_u16mf4_tu (v, base2 + i, vl);
      vuint8mf8_t v3 = __riscv_vnclipu_wx_u8mf8_m(m,v2,31,vl);
      __riscv_vse8_v_u8mf8 (out + 100*i,v3,vl);
    }
}

void f19 (void *base,void *base2,void *out,size_t vl, int n, size_t shift)
{
    vuint32m4_t src = __riscv_vle32_v_u32m4 (base + 100, vl);
    for (int i = 0; i < n; i++){
      vbool8_t m = __riscv_vlm_v_b8 (base + i, vl);
      vuint16m2_t v = __riscv_vnclipu_wx_u16m2_m(m,src,31,vl);
      vuint16m2_t v2 = __riscv_vle16_v_u16m2_tu (v, base2 + i, vl);
      vuint8m1_t v3 = __riscv_vnclipu_wx_u8m1_m(m,v2,31,vl);
      vuint8m1_t v4 = __riscv_vnclipu_wx_u8m1_tumu(m,v3,v2,31,vl);
      __riscv_vse8_v_u8m1 (out + 100*i,v3,vl);
      __riscv_vse8_v_u8m1 (out + 222*i,v4,vl);
    }
}

void f20 (int16_t *base,int8_t *out,size_t vl, size_t shift)
{
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base, vl);
    /* Only allow load v30,v31.  */
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29");

    vuint8m1_t v = __riscv_vnclipu_wx_u8m1(src,31,vl);
    /* Only allow vncvt SRC == DEST v30.  */
    asm volatile("#" ::                                                        
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", 
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17",     
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",     
		   "v26", "v27", "v28", "v29", "v31");

    __riscv_vse8_v_u8m1 (out,v,vl);
}

void f21 (int16_t *base,int8_t *out,size_t vl, size_t shift)
{
    vuint16m1_t src = __riscv_vle16_v_u16m1 (base, vl);
    /* Only allow load v31.  */
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v30");

    vuint8mf2_t v = __riscv_vnclipu_wx_u8mf2(src,31,vl);
    /* Only allow vncvt SRC == DEST v31.  */
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v30");

    __riscv_vse8_v_u8mf2 (out,v,vl);
}

void f22 (int16_t *base,int8_t *out,size_t vl, size_t shift)
{
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base, vl);
    /* Only allow load v30,v31.  */
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29");

    vuint8m1_t v = __riscv_vnclipu_wx_u8m1(src,31,vl);
    /* Only allow v29.  */
    asm volatile("#" ::                                                        
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", 
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17",     
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",     
		   "v26", "v27", "v28", "v30", "v31");
    v = __riscv_vadd_vv_u8m1 (v,v,vl);
    /* Only allow v29.  */
    asm volatile("#" ::                                                        
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", 
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17",     
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",     
		   "v26", "v27", "v28", "v30", "v31");

    __riscv_vse8_v_u8m1 (out,v,vl);
}

/* { dg-final { scan-assembler-not {vmv} } } */
/* { dg-final { scan-assembler-not {csrr} } } */

/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f0 (uint16_t *base,uint8_t *out,size_t vl, size_t shift)
{
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base, vl);
    vuint8m1_t v = __riscv_vnclipu_wx_u8m1(src,shift,vl);
    v = __riscv_vnclipu_wv_u8m1(src,v,vl);
    __riscv_vse8_v_u8m1 (out,v,vl);
}

void f1 (uint16_t *base,uint8_t *out,size_t vl, size_t shift)
{
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base, vl);
    vuint8m1_t v = __riscv_vnclipu_wx_u8m1(src,shift,vl);
    v = __riscv_vnclipu_wv_u8m1(src,v,vl);
    __riscv_vse8_v_u8m1 (out,v,vl);
}

void f2 (void *base,void *out,size_t vl, size_t shift)
{
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base, vl);
    vuint8m1_t v = __riscv_vnclipu_wx_u8m1(src,shift,vl);
    v = __riscv_vnclipu_wv_u8m1_tu(v,src,v,vl);
    __riscv_vse8_v_u8m1 (out,v,vl);
    __riscv_vse16_v_u16m2 (out+100,src,vl);
}

void f3 (void *base,void *out,size_t vl, size_t shift)
{
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base, vl);
    vuint8m1_t v = __riscv_vnclipu_wx_u8m1(src,shift,vl);
    v = __riscv_vnclipu_wv_u8m1(src,v,vl);
    __riscv_vse8_v_u8m1 (out,v,vl);
    __riscv_vse16_v_u16m2 (out+100,src,vl);
}

void f4 (void *base,void *out,size_t vl, size_t shift)
{
    vbool8_t m = __riscv_vlm_v_b8 (base + 500, vl);
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base, vl);
    vuint8m1_t v = __riscv_vnclipu_wx_u8m1(src,shift,vl);
    v = __riscv_vnclipu_wv_u8m1_tumu(m,v,src,v,vl);
    __riscv_vse8_v_u8m1 (out,v,vl);
    __riscv_vse16_v_u16m2 (out+100,src,vl);
}

void f5 (void *base,void *out,size_t vl, size_t shift)
{
    vbool8_t m = __riscv_vlm_v_b8 (base + 500, vl);
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base, vl);
    vuint8m1_t v = __riscv_vnclipu_wx_u8m1(src,shift,vl);
    v = __riscv_vnclipu_wv_u8m1_m(m,src,v,vl);
    __riscv_vse8_v_u8m1 (out,v,vl);
    __riscv_vse16_v_u16m2 (out+100,src,vl);
}

void f6 (void *base,void *out,size_t vl, size_t shift)
{
    vbool8_t m = __riscv_vlm_v_b8 (base + 500, vl);
    vuint8m1_t v = __riscv_vle8_v_u8m1 (base + 600, vl);
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base, vl);
    vuint8m1_t v2 = __riscv_vnclipu_wv_u8m1_m(m,src,v,vl);
    __riscv_vse8_v_u8m1 (out,v2,vl);
    __riscv_vse8_v_u8m1 (out+100,v,vl);
}

void f7 (void *base,void *out,size_t vl, size_t shift)
{
    vuint8m1_t v = __riscv_vle8_v_u8m1 (base + 600, vl);
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base, vl);
    vuint8m1_t v2 = __riscv_vnclipu_wx_u8m1(src,shift,vl);
    v2 = __riscv_vnclipu_wv_u8m1 (src,v,vl);
    __riscv_vse8_v_u8m1 (out,v2,vl);
    __riscv_vse8_v_u8m1 (out+100,v,vl);
}

void f8 (void *base,void *out,size_t vl, size_t shift)
{
    vuint8m1_t v = __riscv_vle8_v_u8m1 (base + 600, vl);
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base, vl);
    vuint8m1_t v2 = __riscv_vnclipu_wx_u8m1(src,shift,vl);
    v2 = __riscv_vnclipu_wv_u8m1 (src,v,vl);
    __riscv_vse8_v_u8m1 (out,v2,vl);
    __riscv_vse8_v_u8m1 (out+100,v,vl);
    __riscv_vse16_v_u16m2 (out+200,src,vl);
}

void f9 (void *base,void *out,size_t vl, size_t shift)
{
    vuint8m1_t v = __riscv_vle8_v_u8m1 (base + 600, vl);
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base, vl);
    vuint8m1_t v2 = __riscv_vnclipu_wx_u8m1(src,shift,vl);
    v2 = __riscv_vnclipu_wv_u8m1_tu (v2,src,v,vl);
    __riscv_vse8_v_u8m1 (out,v2,vl);
    __riscv_vse8_v_u8m1 (out+100,v,vl);
    __riscv_vse16_v_u16m2 (out+200,src,vl);
}

void f10 (void *base,void *out,size_t vl, size_t shift)
{
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base, vl);
    vuint8m1_t v = __riscv_vnclipu_wx_u8m1(src,shift,vl);
    v = __riscv_vnclipu_wv_u8m1_tu(v,src,v,vl);
    v = __riscv_vnclipu_wv_u8m1_tu(v,src,v,vl);
    v = __riscv_vnclipu_wv_u8m1_tu(v,src,v,vl);
    v = __riscv_vnclipu_wv_u8m1_tu(v,src,v,vl);
    __riscv_vse8_v_u8m1 (out,v,vl);
    __riscv_vse16_v_u16m2 (out+100,src,vl);
}

void f11 (void *base,void *out,size_t vl, size_t shift)
{
    vuint8m1_t v = __riscv_vle8_v_u8m1 (base + 600, vl);
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base, vl);
    vuint8m1_t v2 = __riscv_vnclipu_wx_u8m1(src,shift,vl);
    v2 = __riscv_vnclipu_wv_u8m1_tu (v2,src,v,vl);
    v2 = __riscv_vnclipu_wv_u8m1_tu (v2,src,v,vl);
    v2 = __riscv_vnclipu_wv_u8m1_tu (v2,src,v,vl);
    v2 = __riscv_vnclipu_wv_u8m1_tu (v2,src,v,vl);
    v2 = __riscv_vnclipu_wv_u8m1_tu (v2,src,v,vl);
    __riscv_vse8_v_u8m1 (out,v2,vl);
    __riscv_vse8_v_u8m1 (out+100,v,vl);
    __riscv_vse16_v_u16m2 (out+200,src,vl);
}

void f12 (void *base,void *out,size_t vl, size_t shift)
{
    vuint8m1_t v = __riscv_vle8_v_u8m1 (base + 600, vl);
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base, vl);
    vuint8m1_t v2 = __riscv_vnclipu_wx_u8m1(src,shift,vl);
    v2 = __riscv_vnclipu_wv_u8m1(src,v2,vl);
    v2 = __riscv_vnclipu_wv_u8m1(src,v2,vl);
    v2 = __riscv_vnclipu_wv_u8m1(src,v2,vl);
    v2 = __riscv_vnclipu_wv_u8m1 (src,v2,vl);
    __riscv_vse8_v_u8m1 (out,v2,vl);
    __riscv_vse8_v_u8m1 (out+100,v,vl);
}

void f13 (void *base,void *base2,void *out,size_t vl, int n)
{
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base + 100, vl);
    for (int i = 0; i < n; i++){
      vbool8_t m = __riscv_vlm_v_b8 (base + i, vl);
      vuint8m1_t v = __riscv_vnclipu_wx_u8m1_m(m,src,vl,vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src,v,vl);
      v = __riscv_vle8_v_u8m1_tu (v, base2, vl);
      __riscv_vse8_v_u8m1 (out + 100*i,v,vl);
    }
}

void f14 (void *base,void *base2,void *out,size_t vl, int n)
{
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base + 100, vl);
    for (int i = 0; i < n; i++){
      vbool8_t m = __riscv_vlm_v_b8 (base + i, vl);
      vuint8m1_t v = __riscv_vle8_v_u8m1 (base + 600, vl);
      vuint8m1_t v2 = __riscv_vnclipu_wv_u8m1(src,v,vl);
      v = __riscv_vle8_v_u8m1_tu (v, base2, vl);
      __riscv_vse8_v_u8m1 (out + 100*i,v,vl);
      __riscv_vse8_v_u8m1 (out + 100*i,v2,vl);
    }
}

void f15 (void *base,void *base2,void *out,size_t vl, int n)
{
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base + 100, vl);
    for (int i = 0; i < n; i++){
      vbool8_t m = __riscv_vlm_v_b8 (base + i, vl);
      vuint8m1_t v = __riscv_vle8_v_u8m1 (base + 600, vl);
      vuint8m1_t v2 = __riscv_vnclipu_wv_u8m1(src,v,vl);
      v = __riscv_vnclipu_wv_u8m1(src,v,vl);
      v = __riscv_vnclipu_wv_u8m1(src,v,vl);
      v = __riscv_vnclipu_wv_u8m1(src,v,vl);
      v = __riscv_vnclipu_wv_u8m1(src,v,vl);
      v = __riscv_vle8_v_u8m1_tu (v, base2, vl);
      __riscv_vse8_v_u8m1 (out + 100*i,v,vl);
      __riscv_vse8_v_u8m1 (out + 100*i,v2,vl);
    }
}

void f16 (uint16_t *base,uint8_t *out,size_t vl, size_t shift)
{
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base, vl);
    vuint8m1_t v = __riscv_vncvt_x_x_w_u8m1(src,vl);
    vuint8m1_t v3 = __riscv_vnclipu_wv_u8m1(src,v,vl);
    __riscv_vse8_v_u8m1 (out,v,vl);
    __riscv_vse8_v_u8m1 (out + 100,v3,vl);
}

void f17 (void *base,void *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vuint16m2_t src = __riscv_vle16_v_u16m2 (base + 100*i, vl);
      vuint8m1_t src2 = __riscv_vle8_v_u8m1 (base + 200*i, vl);
      vuint8m1_t v = __riscv_vnclipu_wv_u8m1(src,src2,vl);
      vuint16m2_t v2 = __riscv_vadd_vv_u16m2 (src, src,vl);
      asm volatile ("":::"memory");
      __riscv_vse8_v_u8m1 (out + 100*i,v,vl);
      __riscv_vse16_v_u16m2 ((out + 200*i),src,vl);
      __riscv_vse8_v_u8m1 ((out + 300*i),src2,vl);
    }
}

void f18 (void *base,void *out,size_t vl, int n)
{
    vuint8m1_t v = __riscv_vle8_v_u8m1 ((base + 1000), vl);
    for (int i = 0; i < n; i++){
      vuint16m2_t src = __riscv_vle16_v_u16m2 (base + 100*i, vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src,v,vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src,v,vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src,v,vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src,v,vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src,v,vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src,v,vl);
      __riscv_vse8_v_u8m1 (out + 100*i,v,vl);
    }
}

void f19 (void *base,void *out,size_t vl, int n)
{
    vuint8m1_t v = __riscv_vle8_v_u8m1 ((base + 1000), vl);
    for (int i = 0; i < n; i++){
      vuint16m2_t src = __riscv_vle16_v_u16m2 (base + 100*i, vl);
      v = __riscv_vnclipu_wv_u8m1(src,v,vl);
      vuint8m1_t v2 = __riscv_vnclipu_wv_u8m1(src,v,vl);
      v2 = __riscv_vnclipu_wv_u8m1(src,v2,vl);
      v2 = __riscv_vnclipu_wv_u8m1(src,v2,vl);
      v2 = __riscv_vnclipu_wv_u8m1(src,v2,vl);
      v2 = __riscv_vnclipu_wv_u8m1(src,v2,vl);
      __riscv_vse8_v_u8m1 (out + 100*i,v,vl);
      __riscv_vse8_v_u8m1 (out + 200*i,v2,vl);
    }
}

void f20 (void *base,void *out,size_t vl, int n)
{
    vuint8m1_t v = __riscv_vle8_v_u8m1 ((base + 1000), vl);
    for (int i = 0; i < n; i++){
      vuint16m2_t src = __riscv_vle16_v_u16m2 (base + 100*i, vl);
      v = __riscv_vnclipu_wv_u8m1(src,v,vl);
      vuint8m1_t v2 = __riscv_vnclipu_wv_u8m1(src,v,vl);
      v2 = __riscv_vnclipu_wv_u8m1(src,v2,vl);
      __riscv_vse8_v_u8m1 (out + 100*i,v,vl);
      __riscv_vse8_v_u8m1 (out + 200*i,v2,vl);
    }
}

void f21 (void *base,void *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vuint8m1_t v = __riscv_vle8_v_u8m1 ((base + 1000 * i), vl);
      vuint16m2_t src = __riscv_vle16_v_u16m2 (base + 100*i, vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src,v,vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src,v,vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src,v,vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src,v,vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src,v,vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src,v,vl);
      __riscv_vse8_v_u8m1 (out + 100*i,v,vl);
      __riscv_vse16_v_u16m2 (out + 200*i,src,vl);
    }
}

void f22 (uint16_t *base,uint8_t *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vuint8m1_t v = __riscv_vle8_v_u8m1 ((uint8_t *)(base + 1000 * i), vl);
      vuint16m2_t src1 = __riscv_vle16_v_u16m2 (base + 100*i, vl);
      vuint16m2_t src2 = __riscv_vle16_v_u16m2 (base + 200*i, vl);
      vuint16m2_t src3 = __riscv_vle16_v_u16m2 (base + 300*i, vl);
      vuint16m2_t src4 = __riscv_vle16_v_u16m2 (base + 400*i, vl);
      vuint16m2_t src5 = __riscv_vle16_v_u16m2 (base + 500*i, vl);
      vuint16m2_t src6 = __riscv_vle16_v_u16m2 (base + 600*i, vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src1,v,vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src2,v,vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src3,v,vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src4,v,vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src5,v,vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src6,v,vl);
      __riscv_vse8_v_u8m1 (out + 100*i,v,vl);
    }
}

void f23 (uint16_t *base,uint8_t *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vuint8m1_t v = __riscv_vle8_v_u8m1 ((uint8_t *)(base + 1000 * i), vl);
      vuint8m1_t v2 = __riscv_vle8_v_u8m1 ((uint8_t *)(base + 2000 * i), vl);
      vuint16m2_t src1 = __riscv_vle16_v_u16m2 (base + 100*i, vl);
      vuint16m2_t src2 = __riscv_vle16_v_u16m2 (base + 200*i, vl);
      vuint16m2_t src3 = __riscv_vle16_v_u16m2 (base + 300*i, vl);
      vuint16m2_t src4 = __riscv_vle16_v_u16m2 (base + 400*i, vl);
      vuint16m2_t src5 = __riscv_vle16_v_u16m2 (base + 500*i, vl);
      vuint16m2_t src6 = __riscv_vle16_v_u16m2 (base + 600*i, vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src1,v2,vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src2,v2,vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src3,v2,vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src4,v2,vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src5,v2,vl);
      v = __riscv_vnclipu_wv_u8m1_tu(v,src6,v2,vl);
      __riscv_vse8_v_u8m1 (out + 100*i,v,vl);
    }
}

void f24 (void *base,void *base2,void *out,size_t vl, int n)
{
    vuint32m4_t src = __riscv_vle32_v_u32m4 (base + 100, vl);
    vuint16m2_t src2 = __riscv_vle16_v_u16m2 (base + 200, vl);
    vuint8m1_t src3 = __riscv_vle8_v_u8m1 (base + 300, vl);
    for (int i = 0; i < n; i++){
      vbool8_t m = __riscv_vlm_v_b8 (base + i, vl);
      vuint16m2_t v = __riscv_vnclipu_wv_u16m2_m(m,src,src2,vl);
      vuint16m2_t v2 = __riscv_vle16_v_u16m2_tu (v, base2 + i, vl);
      vuint8m1_t v3 = __riscv_vnclipu_wv_u8m1_m(m,v2,src3,vl);
      __riscv_vse8_v_u8m1 (out + 100*i,v3,vl);
    }
}

void f25 (void *base,void *out,size_t vl, size_t shift)
{
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base, vl);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29");
    vuint8m1_t v = __riscv_vle8_v_u8m1 (base + 100, vl);
    vuint8m1_t v2 = __riscv_vnclipu_wv_u8m1(src,v,vl);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v31");
    __riscv_vse8_v_u8m1 (out,v2,vl);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v31");
}

void f26 (void *base,void *out,size_t vl, size_t shift)
{
    vuint8m1_t v = __riscv_vle8_v_u8m1 (base + 100, vl);
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base, vl);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28");
    vuint8m1_t v2 = __riscv_vnclipu_wv_u8m1(src,v,vl);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v30", "v31");
    __riscv_vse8_v_u8m1 (out,v2,vl);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v30", "v31");
}

void f27 (void *base,void *out,size_t vl, size_t shift)
{
    vuint8m1_t v = __riscv_vle8_v_u8m1 (base + 100, vl);
    vuint16m2_t src = __riscv_vle16_v_u16m2 (base, vl);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28");
    vuint8m1_t v2 = __riscv_vnclipu_wv_u8m1(src,v,vl);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v29", "v28", "v30", "v31");
    __riscv_vse8_v_u8m1 (out,v2,vl);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v29", "v28", "v30", "v31");
}

/* { dg-final { scan-assembler-not {vmv} } } */
/* { dg-final { scan-assembler-not {csrr} } } */

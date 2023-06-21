/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f0 (uint16_t *base,uint8_t *out,size_t vl, size_t shift)
{
    vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base, vl);
    vuint8mf8_t v = __riscv_vnclipu_wx_u8mf8(src,shift,0,vl);
    v = __riscv_vnclipu_wv_u8mf8(src,v,0,vl);
    __riscv_vse8_v_u8mf8 (out,v,vl);
}

void f1 (uint16_t *base,uint8_t *out,size_t vl, size_t shift)
{
    vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base, vl);
    vuint8mf8_t v = __riscv_vnclipu_wx_u8mf8(src,shift,0,vl);
    v = __riscv_vnclipu_wv_u8mf8(src,v,0,vl);
    __riscv_vse8_v_u8mf8 (out,v,vl);
}

void f2 (void *base,void *out,size_t vl, size_t shift)
{
    vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base, vl);
    vuint8mf8_t v = __riscv_vnclipu_wx_u8mf8(src,shift,0,vl);
    v = __riscv_vnclipu_wv_u8mf8_tu(v,src,v,0,vl);
    __riscv_vse8_v_u8mf8 (out,v,vl);
    __riscv_vse16_v_u16mf4 (out+100,src,vl);
}

void f3 (void *base,void *out,size_t vl, size_t shift)
{
    vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base, vl);
    vuint8mf8_t v = __riscv_vnclipu_wx_u8mf8(src,shift,0,vl);
    v = __riscv_vnclipu_wv_u8mf8(src,v,0,vl);
    __riscv_vse8_v_u8mf8 (out,v,vl);
    __riscv_vse16_v_u16mf4 (out+100,src,vl);
}

void f4 (void *base,void *out,size_t vl, size_t shift)
{
    vbool64_t m = __riscv_vlm_v_b64 (base + 500, vl);
    vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base, vl);
    vuint8mf8_t v = __riscv_vnclipu_wx_u8mf8(src,shift,0,vl);
    v = __riscv_vnclipu_wv_u8mf8_tumu(m,v,src,v,0,vl);
    __riscv_vse8_v_u8mf8 (out,v,vl);
    __riscv_vse16_v_u16mf4 (out+100,src,vl);
}

void f5 (void *base,void *out,size_t vl, size_t shift)
{
    vbool64_t m = __riscv_vlm_v_b64 (base + 500, vl);
    vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base, vl);
    vuint8mf8_t v = __riscv_vnclipu_wx_u8mf8(src,shift,0,vl);
    v = __riscv_vnclipu_wv_u8mf8_m(m,src,v,0,vl);
    __riscv_vse8_v_u8mf8 (out,v,vl);
    __riscv_vse16_v_u16mf4 (out+100,src,vl);
}

void f6 (void *base,void *out,size_t vl, size_t shift)
{
    vbool64_t m = __riscv_vlm_v_b64 (base + 500, vl);
    vuint8mf8_t v = __riscv_vle8_v_u8mf8 (base + 600, vl);
    vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base, vl);
    vuint8mf8_t v2 = __riscv_vnclipu_wv_u8mf8_m(m,src,v,0,vl);
    __riscv_vse8_v_u8mf8 (out,v2,vl);
    __riscv_vse8_v_u8mf8 (out+100,v,vl);
}

void f7 (void *base,void *out,size_t vl, size_t shift)
{
    vuint8mf8_t v = __riscv_vle8_v_u8mf8 (base + 600, vl);
    vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base, vl);
    vuint8mf8_t v2 = __riscv_vnclipu_wx_u8mf8(src,shift,0,vl);
    v2 = __riscv_vnclipu_wv_u8mf8 (src,v,0,vl);
    __riscv_vse8_v_u8mf8 (out,v2,vl);
    __riscv_vse8_v_u8mf8 (out+100,v,vl);
}

void f8 (void *base,void *out,size_t vl, size_t shift)
{
    vuint8mf8_t v = __riscv_vle8_v_u8mf8 (base + 600, vl);
    vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base, vl);
    vuint8mf8_t v2 = __riscv_vnclipu_wx_u8mf8(src,shift,0,vl);
    v2 = __riscv_vnclipu_wv_u8mf8 (src,v,0,vl);
    __riscv_vse8_v_u8mf8 (out,v2,vl);
    __riscv_vse8_v_u8mf8 (out+100,v,vl);
    __riscv_vse16_v_u16mf4 (out+200,src,vl);
}

void f9 (void *base,void *out,size_t vl, size_t shift)
{
    vuint8mf8_t v = __riscv_vle8_v_u8mf8 (base + 600, vl);
    vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base, vl);
    vuint8mf8_t v2 = __riscv_vnclipu_wx_u8mf8(src,shift,0,vl);
    v2 = __riscv_vnclipu_wv_u8mf8_tu (v2,src,v,0,vl);
    __riscv_vse8_v_u8mf8 (out,v2,vl);
    __riscv_vse8_v_u8mf8 (out+100,v,vl);
    __riscv_vse16_v_u16mf4 (out+200,src,vl);
}

void f10 (void *base,void *out,size_t vl, size_t shift)
{
    vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base, vl);
    vuint8mf8_t v = __riscv_vnclipu_wx_u8mf8(src,shift,0,vl);
    v = __riscv_vnclipu_wv_u8mf8_tu(v,src,v,0,vl);
    v = __riscv_vnclipu_wv_u8mf8_tu(v,src,v,0,vl);
    v = __riscv_vnclipu_wv_u8mf8_tu(v,src,v,0,vl);
    v = __riscv_vnclipu_wv_u8mf8_tu(v,src,v,0,vl);
    __riscv_vse8_v_u8mf8 (out,v,vl);
    __riscv_vse16_v_u16mf4 (out+100,src,vl);
}

void f11 (void *base,void *out,size_t vl, size_t shift)
{
    vuint8mf8_t v = __riscv_vle8_v_u8mf8 (base + 600, vl);
    vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base, vl);
    vuint8mf8_t v2 = __riscv_vnclipu_wx_u8mf8(src,shift,0,vl);
    v2 = __riscv_vnclipu_wv_u8mf8_tu (v2,src,v,0,vl);
    v2 = __riscv_vnclipu_wv_u8mf8_tu (v2,src,v,0,vl);
    v2 = __riscv_vnclipu_wv_u8mf8_tu (v2,src,v,0,vl);
    v2 = __riscv_vnclipu_wv_u8mf8_tu (v2,src,v,0,vl);
    v2 = __riscv_vnclipu_wv_u8mf8_tu (v2,src,v,0,vl);
    __riscv_vse8_v_u8mf8 (out,v2,vl);
    __riscv_vse8_v_u8mf8 (out+100,v,vl);
    __riscv_vse16_v_u16mf4 (out+200,src,vl);
}

void f12 (void *base,void *out,size_t vl, size_t shift)
{
    vuint8mf8_t v = __riscv_vle8_v_u8mf8 (base + 600, vl);
    vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base, vl);
    vuint8mf8_t v2 = __riscv_vnclipu_wx_u8mf8(src,shift,0,vl);
    v2 = __riscv_vnclipu_wv_u8mf8(src,v2,0,vl);
    v2 = __riscv_vnclipu_wv_u8mf8(src,v2,0,vl);
    v2 = __riscv_vnclipu_wv_u8mf8(src,v2,0,vl);
    v2 = __riscv_vnclipu_wv_u8mf8 (src,v2,0,vl);
    __riscv_vse8_v_u8mf8 (out,v2,vl);
    __riscv_vse8_v_u8mf8 (out+100,v,vl);
}

void f13 (void *base,void *base2,void *out,size_t vl, int n)
{
    vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base + 100, vl);
    for (int i = 0; i < n; i++){
      vbool64_t m = __riscv_vlm_v_b64 (base + i, vl);
      vuint8mf8_t v = __riscv_vnclipu_wx_u8mf8_m(m,src,vl,0,vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src,v,0,vl);
      v = __riscv_vle8_v_u8mf8_tu (v, base2, vl);
      __riscv_vse8_v_u8mf8 (out + 100*i,v,vl);
    }
}

void f14 (void *base,void *base2,void *out,size_t vl, int n)
{
    vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base + 100, vl);
    for (int i = 0; i < n; i++){
      vbool64_t m = __riscv_vlm_v_b64 (base + i, vl);
      vuint8mf8_t v = __riscv_vle8_v_u8mf8 (base + 600, vl);
      vuint8mf8_t v2 = __riscv_vnclipu_wv_u8mf8(src,v,0,vl);
      v = __riscv_vle8_v_u8mf8_tu (v, base2, vl);
      __riscv_vse8_v_u8mf8 (out + 100*i,v,vl);
      __riscv_vse8_v_u8mf8 (out + 100*i,v2,vl);
    }
}

void f15 (void *base,void *base2,void *out,size_t vl, int n)
{
    vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base + 100, vl);
    for (int i = 0; i < n; i++){
      vbool64_t m = __riscv_vlm_v_b64 (base + i, vl);
      vuint8mf8_t v = __riscv_vle8_v_u8mf8 (base + 600, vl);
      vuint8mf8_t v2 = __riscv_vnclipu_wv_u8mf8(src,v,0,vl);
      v = __riscv_vnclipu_wv_u8mf8(src,v,0,vl);
      v = __riscv_vnclipu_wv_u8mf8(src,v,0,vl);
      v = __riscv_vnclipu_wv_u8mf8(src,v,0,vl);
      v = __riscv_vnclipu_wv_u8mf8(src,v,0,vl);
      v = __riscv_vle8_v_u8mf8_tu (v, base2, vl);
      __riscv_vse8_v_u8mf8 (out + 100*i,v,vl);
      __riscv_vse8_v_u8mf8 (out + 100*i,v2,vl);
    }
}

void f16 (uint16_t *base,uint8_t *out,size_t vl, size_t shift)
{
    vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base, vl);
    vuint8mf8_t v = __riscv_vncvt_x_x_w_u8mf8(src,vl);
    vuint8mf8_t v3 = __riscv_vnclipu_wv_u8mf8(src,v,0,vl);
    __riscv_vse8_v_u8mf8 (out,v,vl);
    __riscv_vse8_v_u8mf8 (out + 100,v3,vl);
}

void f17 (void *base,void *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base + 100*i, vl);
      vuint8mf8_t src2 = __riscv_vle8_v_u8mf8 (base + 200*i, vl);
      vuint8mf8_t v = __riscv_vnclipu_wv_u8mf8(src,src2,0,vl);
      vuint16mf4_t v2 = __riscv_vadd_vv_u16mf4 (src, src,vl);
      asm volatile ("":::"memory");
      __riscv_vse8_v_u8mf8 (out + 100*i,v,vl);
      __riscv_vse16_v_u16mf4 ((out + 200*i),src,vl);
      __riscv_vse8_v_u8mf8 ((out + 300*i),src2,vl);
    }
}

void f18 (void *base,void *out,size_t vl, int n)
{
    vuint8mf8_t v = __riscv_vle8_v_u8mf8 ((base + 1000), vl);
    for (int i = 0; i < n; i++){
      vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base + 100*i, vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src,v,0,vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src,v,0,vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src,v,0,vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src,v,0,vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src,v,0,vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src,v,0,vl);
      __riscv_vse8_v_u8mf8 (out + 100*i,v,vl);
    }
}

void f19 (void *base,void *out,size_t vl, int n)
{
    vuint8mf8_t v = __riscv_vle8_v_u8mf8 ((base + 1000), vl);
    for (int i = 0; i < n; i++){
      vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base + 100*i, vl);
      v = __riscv_vnclipu_wv_u8mf8(src,v,0,vl);
      vuint8mf8_t v2 = __riscv_vnclipu_wv_u8mf8(src,v,0,vl);
      v2 = __riscv_vnclipu_wv_u8mf8(src,v2,0,vl);
      v2 = __riscv_vnclipu_wv_u8mf8(src,v2,0,vl);
      v2 = __riscv_vnclipu_wv_u8mf8(src,v2,0,vl);
      v2 = __riscv_vnclipu_wv_u8mf8(src,v2,0,vl);
      __riscv_vse8_v_u8mf8 (out + 100*i,v,vl);
      __riscv_vse8_v_u8mf8 (out + 200*i,v2,vl);
    }
}

void f20 (void *base,void *out,size_t vl, int n)
{
    vuint8mf8_t v = __riscv_vle8_v_u8mf8 ((base + 1000), vl);
    for (int i = 0; i < n; i++){
      vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base + 100*i, vl);
      v = __riscv_vnclipu_wv_u8mf8(src,v,0,vl);
      vuint8mf8_t v2 = __riscv_vnclipu_wv_u8mf8(src,v,0,vl);
      v2 = __riscv_vnclipu_wv_u8mf8(src,v2,0,vl);
      __riscv_vse8_v_u8mf8 (out + 100*i,v,vl);
      __riscv_vse8_v_u8mf8 (out + 200*i,v2,vl);
    }
}

void f21 (void *base,void *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vuint8mf8_t v = __riscv_vle8_v_u8mf8 ((base + 1000 * i), vl);
      vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base + 100*i, vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src,v,0,vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src,v,0,vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src,v,0,vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src,v,0,vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src,v,0,vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src,v,0,vl);
      __riscv_vse8_v_u8mf8 (out + 100*i,v,vl);
      __riscv_vse16_v_u16mf4 (out + 200*i,src,vl);
    }
}

void f22 (uint16_t *base,uint8_t *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vuint8mf8_t v = __riscv_vle8_v_u8mf8 ((uint8_t *)(base + 1000 * i), vl);
      vuint16mf4_t src1 = __riscv_vle16_v_u16mf4 (base + 100*i, vl);
      vuint16mf4_t src2 = __riscv_vle16_v_u16mf4 (base + 200*i, vl);
      vuint16mf4_t src3 = __riscv_vle16_v_u16mf4 (base + 300*i, vl);
      vuint16mf4_t src4 = __riscv_vle16_v_u16mf4 (base + 400*i, vl);
      vuint16mf4_t src5 = __riscv_vle16_v_u16mf4 (base + 500*i, vl);
      vuint16mf4_t src6 = __riscv_vle16_v_u16mf4 (base + 600*i, vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src1,v,0,vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src2,v,0,vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src3,v,0,vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src4,v,0,vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src5,v,0,vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src6,v,0,vl);
      __riscv_vse8_v_u8mf8 (out + 100*i,v,vl);
    }
}

void f23 (uint16_t *base,uint8_t *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vuint8mf8_t v = __riscv_vle8_v_u8mf8 ((uint8_t *)(base + 1000 * i), vl);
      vuint8mf8_t v2 = __riscv_vle8_v_u8mf8 ((uint8_t *)(base + 2000 * i), vl);
      vuint16mf4_t src1 = __riscv_vle16_v_u16mf4 (base + 100*i, vl);
      vuint16mf4_t src2 = __riscv_vle16_v_u16mf4 (base + 200*i, vl);
      vuint16mf4_t src3 = __riscv_vle16_v_u16mf4 (base + 300*i, vl);
      vuint16mf4_t src4 = __riscv_vle16_v_u16mf4 (base + 400*i, vl);
      vuint16mf4_t src5 = __riscv_vle16_v_u16mf4 (base + 500*i, vl);
      vuint16mf4_t src6 = __riscv_vle16_v_u16mf4 (base + 600*i, vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src1,v2,0,vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src2,v2,0,vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src3,v2,0,vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src4,v2,0,vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src5,v2,0,vl);
      v = __riscv_vnclipu_wv_u8mf8_tu(v,src6,v2,0,vl);
      __riscv_vse8_v_u8mf8 (out + 100*i,v,vl);
    }
}

void f24 (void *base,void *base2,void *out,size_t vl, int n)
{
    vuint32mf2_t src = __riscv_vle32_v_u32mf2 (base + 100, vl);
    vuint16mf4_t src2 = __riscv_vle16_v_u16mf4 (base + 200, vl);
    vuint8mf8_t src3 = __riscv_vle8_v_u8mf8 (base + 300, vl);
    for (int i = 0; i < n; i++){
      vbool64_t m = __riscv_vlm_v_b64 (base + i, vl);
      vuint16mf4_t v = __riscv_vnclipu_wv_u16mf4_m(m,src,src2,0,vl);
      vuint16mf4_t v2 = __riscv_vle16_v_u16mf4_tu (v, base2 + i, vl);
      vuint8mf8_t v3 = __riscv_vnclipu_wv_u8mf8_m(m,v2,src3,0,vl);
      __riscv_vse8_v_u8mf8 (out + 100*i,v3,vl);
    }
}

void f25 (void *base,void *out,size_t vl, size_t shift)
{
    vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base, vl);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v30");
    vuint8mf8_t v = __riscv_vle8_v_u8mf8 (base + 100, vl);
    vuint8mf8_t v2 = __riscv_vnclipu_wv_u8mf8(src,v,0,vl);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v30");
    __riscv_vse8_v_u8mf8 (out,v2,vl);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v30");
}

void f26 (void *base,void *out,size_t vl, size_t shift)
{
    vuint8mf8_t v = __riscv_vle8_v_u8mf8 (base + 100, vl);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v30");
    vuint16mf4_t src = __riscv_vle16_v_u16mf4 (base, vl);
    vuint8mf8_t v2 = __riscv_vnclipu_wv_u8mf8(src,v,0,vl);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v30");
    __riscv_vse8_v_u8mf8 (out,v2,vl);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",  
		   "v26", "v27", "v28", "v29", "v30");
}

/* { dg-final { scan-assembler-not {vmv} } } */
/* { dg-final { scan-assembler-not {csrr} } } */

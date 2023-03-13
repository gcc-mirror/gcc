/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

void f0 (void *base,void *out,size_t vl)
{
    vuint64m1_t bindex = __riscv_vle64_v_u64m1 (base, vl);
    vint8mf8_t v = __riscv_vluxei64_v_i8mf8(base,bindex,vl);
    __riscv_vse8_v_i8mf8 (out,v,vl);
}

void f1 (void *base,void *out,size_t vl)
{
    vuint64m1_t bindex = __riscv_vle64_v_u64m1 (base, vl);
    vint8mf8_t bindex2 = __riscv_vle8_v_i8mf8 ((void *)(base + 100), vl);
    vint8mf8_t v = __riscv_vluxei64_v_i8mf8_tu(bindex2,base,bindex,vl);
    __riscv_vse8_v_i8mf8 (out,v,vl);
}

void f2 (void *base,void *out,size_t vl)
{
    vuint64m1_t bindex = __riscv_vle64_v_u64m1 (base, vl);
    vint8mf8_t v = __riscv_vluxei64_v_i8mf8(base,bindex,vl);
    vuint64m1_t v2 = __riscv_vadd_vv_u64m1 (bindex, bindex,vl);
    __riscv_vse8_v_i8mf8 (out,v,vl);
    __riscv_vse64_v_u64m1 ((void *)out,v2,vl);
}

void f3 (void *base,void *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vuint64m1_t bindex = __riscv_vle64_v_u64m1 (base + 100*i, vl);
      vint8mf8_t v = __riscv_vluxei64_v_i8mf8(base,bindex,vl);
      vuint64m1_t v2 = __riscv_vadd_vv_u64m1 (bindex, bindex,vl);
      __riscv_vse8_v_i8mf8 (out + 100*i,v,vl);
      __riscv_vse64_v_u64m1 ((void *)(out + 200*i),v2,vl);
    }
}

void f4 (void *base,void *out,size_t vl)
{
    vuint64m1_t bindex = __riscv_vle64_v_u64m1 (base, vl);
    vint8mf8_t v = __riscv_vluxei64_v_i8mf8(base,bindex,vl);
    v = __riscv_vluxei64_v_i8mf8_tu(v,base,bindex,vl);
    v = __riscv_vluxei64_v_i8mf8_tu(v,base,bindex,vl);
    vuint64m1_t v2 = __riscv_vadd_vv_u64m1 (bindex, bindex,vl);
    __riscv_vse8_v_i8mf8 (out,v,vl);
    __riscv_vse64_v_u64m1 ((void *)out,v2,vl);
}

void f5 (void *base,void *base2,void *out,size_t vl, int n)
{
    vuint64m1_t bindex = __riscv_vle64_v_u64m1 (base + 100, vl);
    for (int i = 0; i < n; i++){
      vbool64_t m = __riscv_vlm_v_b64 (base + i, vl);
      vint8mf8_t v = __riscv_vluxei64_v_i8mf8_m(m,base,bindex,vl);
      v = __riscv_vluxei64_v_i8mf8_tu(v,base,bindex,vl);
      v = __riscv_vle8_v_i8mf8_tu (v, base2, vl);
      __riscv_vse8_v_i8mf8 (out + 100*i,v,vl);
    }
}

void f6 (void *base,void *out,size_t vl)
{
    vuint64m8_t bindex = __riscv_vle64_v_u64m8 (base, vl);
    vint8m1_t v = __riscv_vluxei64_v_i8m1(base,bindex,vl);
    __riscv_vse8_v_i8m1 (out,v,vl);
}

void f7 (void *base,void *out,size_t vl)
{
    vuint64m8_t bindex = __riscv_vle64_v_u64m8 (base, vl);
    vint8m1_t src = __riscv_vle8_v_i8m1 ((void *)(base + 100), vl);
    vint8m1_t v = __riscv_vluxei64_v_i8m1_tu(src,base,bindex,vl);
    __riscv_vse8_v_i8m1 (out,v,vl);
}

void f8 (void *base,void *out,size_t vl)
{
    vuint64m8_t bindex = __riscv_vle64_v_u64m8 (base, vl);
    vint8m1_t v = __riscv_vluxei64_v_i8m1(base,bindex,vl);
    vuint64m8_t v2 = __riscv_vadd_vv_u64m8 (bindex, bindex,vl);
    __riscv_vse8_v_i8m1 (out,v,vl);
    __riscv_vse64_v_u64m8 ((void *)out,v2,vl);
}

void f9 (void *base,void *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vuint64m8_t bindex = __riscv_vle64_v_u64m8 (base + 100*i, vl);
      vint8m1_t v = __riscv_vluxei64_v_i8m1(base,bindex,vl);
      vuint64m8_t v2 = __riscv_vadd_vv_u64m8 (bindex, bindex,vl);
      __riscv_vse8_v_i8m1 (out + 100*i,v,vl);
      __riscv_vse64_v_u64m8 ((void *)(out + 200*i),v2,vl);
    }
}

void f10 (void *base,void *out,size_t vl)
{
    vuint64m8_t bindex = __riscv_vle64_v_u64m8 (base, vl);
    vint8m1_t v = __riscv_vluxei64_v_i8m1(base,bindex,vl);
    v = __riscv_vluxei64_v_i8m1_tu(v,base,bindex,vl);
    v = __riscv_vluxei64_v_i8m1_tu(v,base,bindex,vl);
    vuint64m8_t v2 = __riscv_vadd_vv_u64m8 (bindex, bindex,vl);
    __riscv_vse8_v_i8m1 (out,v,vl);
    __riscv_vse64_v_u64m8 ((void *)out,v2,vl);
}

void f11 (void *base,void *base2,void *out,size_t vl, int n)
{
    vuint64m8_t bindex = __riscv_vle64_v_u64m8 (base + 100, vl);
    for (int i = 0; i < n; i++){
      vbool8_t m = __riscv_vlm_v_b8 (base + i, vl);
      vint8m1_t v = __riscv_vluxei64_v_i8m1_m(m,base,bindex,vl);
      v = __riscv_vluxei64_v_i8m1_tu(v,base,bindex,vl);
      v = __riscv_vle8_v_i8m1_tu (v, base2, vl);
      __riscv_vse8_v_i8m1 (out + 100*i,v,vl);
    }
}

void f12 (void *base,void *out,size_t vl, int n)
{
    vint8mf8_t v = __riscv_vle8_v_i8mf8 ((void *)(base + 1000), vl);
    for (int i = 0; i < n; i++){
      vuint64m1_t bindex = __riscv_vle64_v_u64m1 (base + 100*i, vl);
      v = __riscv_vluxei64_v_i8mf8_tu(v,base,bindex,vl);
      v = __riscv_vluxei64_v_i8mf8_tu(v,base,bindex,vl);
      v = __riscv_vluxei64_v_i8mf8_tu(v,base,bindex,vl);
      v = __riscv_vluxei64_v_i8mf8_tu(v,base,bindex,vl);
      v = __riscv_vluxei64_v_i8mf8_tu(v,base,bindex,vl);
      v = __riscv_vluxei64_v_i8mf8_tu(v,base,bindex,vl);
      __riscv_vse8_v_i8mf8 (out + 100*i,v,vl);
    }
}

void f13 (void *base,void *out,size_t vl, int n)
{
    vint8m1_t v = __riscv_vle8_v_i8m1 ((void *)(base + 1000), vl);
    for (int i = 0; i < n; i++){
      vuint64m8_t bindex = __riscv_vle64_v_u64m8 (base + 100*i, vl);
      v = __riscv_vluxei64_v_i8m1_tu(v,base,bindex,vl);
      v = __riscv_vluxei64_v_i8m1_tu(v,base,bindex,vl);
      v = __riscv_vluxei64_v_i8m1_tu(v,base,bindex,vl);
      v = __riscv_vluxei64_v_i8m1_tu(v,base,bindex,vl);
      v = __riscv_vluxei64_v_i8m1_tu(v,base,bindex,vl);
      v = __riscv_vluxei64_v_i8m1_tu(v,base,bindex,vl);
      __riscv_vse8_v_i8m1 (out + 100*i,v,vl);
    }
}

void f14 (void *base,void *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vint8mf8_t v = __riscv_vle8_v_i8mf8 ((void *)(base + 1000 * i), vl);
      vuint64m1_t bindex = __riscv_vle64_v_u64m1 (base + 100*i, vl);
      v = __riscv_vluxei64_v_i8mf8_tu(v,base,bindex,vl);
      v = __riscv_vluxei64_v_i8mf8_tu(v,base,bindex,vl);
      v = __riscv_vluxei64_v_i8mf8_tu(v,base,bindex,vl);
      v = __riscv_vluxei64_v_i8mf8_tu(v,base,bindex,vl);
      v = __riscv_vluxei64_v_i8mf8_tu(v,base,bindex,vl);
      v = __riscv_vluxei64_v_i8mf8_tu(v,base,bindex,vl);
      __riscv_vse8_v_i8mf8 (out + 100*i,v,vl);
    }
}

void f15 (void *base,void *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vint8m1_t v = __riscv_vle8_v_i8m1 ((void *)(base + 1000 * i), vl);
      vuint64m8_t bindex = __riscv_vle64_v_u64m8 (base + 100*i, vl);
      v = __riscv_vluxei64_v_i8m1_tu(v,base,bindex,vl);
      v = __riscv_vluxei64_v_i8m1_tu(v,base,bindex,vl);
      v = __riscv_vluxei64_v_i8m1_tu(v,base,bindex,vl);
      v = __riscv_vluxei64_v_i8m1_tu(v,base,bindex,vl);
      v = __riscv_vluxei64_v_i8m1_tu(v,base,bindex,vl);
      v = __riscv_vluxei64_v_i8m1_tu(v,base,bindex,vl);
      __riscv_vse8_v_i8m1 (out + 100*i,v,vl);
    }
}

void f16 (void *base,void *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vint8mf8_t v = __riscv_vle8_v_i8mf8 ((void *)(base + 1000 * i), vl);
      vuint64m1_t bindex1 = __riscv_vle64_v_u64m1 (base + 100*i, vl);
      vuint64m1_t bindex2 = __riscv_vle64_v_u64m1 (base + 200*i, vl);
      vuint64m1_t bindex3 = __riscv_vle64_v_u64m1 (base + 300*i, vl);
      vuint64m1_t bindex4 = __riscv_vle64_v_u64m1 (base + 400*i, vl);
      vuint64m1_t bindex5 = __riscv_vle64_v_u64m1 (base + 500*i, vl);
      vuint64m1_t bindex6 = __riscv_vle64_v_u64m1 (base + 600*i, vl);
      v = __riscv_vluxei64_v_i8mf8_tu(v,base,bindex1,vl);
      v = __riscv_vluxei64_v_i8mf8_tu(v,base,bindex2,vl);
      v = __riscv_vluxei64_v_i8mf8_tu(v,base,bindex3,vl);
      v = __riscv_vluxei64_v_i8mf8_tu(v,base,bindex4,vl);
      v = __riscv_vluxei64_v_i8mf8_tu(v,base,bindex5,vl);
      v = __riscv_vluxei64_v_i8mf8_tu(v,base,bindex6,vl);
      __riscv_vse8_v_i8mf8 (out + 100*i,v,vl);
    }
}

void f17 (void *base,void *out,size_t vl, int n)
{
    for (int i = 0; i < n; i++){
      vint8m1_t v = __riscv_vle8_v_i8m1 ((void *)(base + 1000 * i), vl);
      vuint64m8_t bindex1 = __riscv_vle64_v_u64m8 (base + 100*i, vl);
      vuint64m8_t bindex2 = __riscv_vle64_v_u64m8 (base + 200*i, vl);
      vuint64m8_t bindex3 = __riscv_vle64_v_u64m8 (base + 300*i, vl);
      v = __riscv_vluxei64_v_i8m1_tu(v,base,bindex1,vl);
      v = __riscv_vluxei64_v_i8m1_tu(v,base,bindex2,vl);
      v = __riscv_vluxei64_v_i8m1_tu(v,base,bindex3,vl);
      __riscv_vse8_v_i8m1 (out + 100*i,v,vl);
    }
}

void f18 (void *base,void *base2,void *out,size_t vl, int n)
{
    vuint64m8_t bindex = __riscv_vle64_v_u64m8 (base + 100, vl);
    for (int i = 0; i < n; i++){
      vbool8_t m = __riscv_vlm_v_b8 (base + i, vl);
      vuint32m4_t v = __riscv_vluxei64_v_u32m4_m(m,base,bindex,vl);
      vuint32m4_t v2 = __riscv_vle32_v_u32m4_tu (v, base2 + i, vl);
      vint8m1_t v3 = __riscv_vluxei32_v_i8m1_m(m,base,v2,vl);
      __riscv_vse8_v_i8m1 (out + 100*i,v3,vl);
    }
}

void f19 (void *base,void *base2,void *out,size_t vl, int n)
{
    vuint64m8_t bindex = __riscv_vle64_v_u64m8 (base + 100, vl);
    for (int i = 0; i < n; i++){
      vbool8_t m = __riscv_vlm_v_b8 (base + i, vl);
      vuint64m8_t v = __riscv_vluxei64_v_u64m8_m(m,base,bindex,vl);
      vuint64m8_t v2 = __riscv_vle64_v_u64m8_tu (v, base2 + i, vl);
      vint8m1_t v3 = __riscv_vluxei64_v_i8m1_m(m,base,v,vl);
      vint8m1_t v4 = __riscv_vluxei64_v_i8m1_m(m,base,v2,vl);
      __riscv_vse8_v_i8m1 (out + 100*i,v3,vl);
      __riscv_vse8_v_i8m1 (out + 222*i,v4,vl);
    }
}
void f20 (void *base,void *out,size_t vl)
{
    vuint64m8_t bindex = __riscv_vle64_v_u64m8 (base, vl);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23");

    vint8m1_t v = __riscv_vluxei64_v_i8m1(base,bindex,vl);
    asm volatile("#" ::                                                        
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", 
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17",     
		   "v18", "v19", "v20", "v21", "v22", "v23", "v25",     
		   "v26", "v27", "v28", "v29", "v30", "v31");

    __riscv_vse8_v_i8m1 (out,v,vl);
}

void f21 (void *base,void *out,size_t vl)
{
    vuint64m8_t bindex = __riscv_vle64_v_u64m8 (base, vl);
    vbool8_t m = __riscv_vlm_v_b8 (base, vl);
    asm volatile("#" ::
		 : "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23");

    vint8m1_t v = __riscv_vluxei64_v_i8m1_m(m,base,bindex,vl);
    asm volatile("#" ::                                                        
		 : "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", 
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17",     
		   "v18", "v19", "v20", "v21", "v22", "v23", "v25",     
		   "v26", "v27", "v28", "v29", "v30", "v31");

    __riscv_vse8_v_i8m1 (out,v,vl);
}

void f22 (void *base,void *out,size_t vl)
{
    vuint64m8_t bindex = __riscv_vle64_v_u64m8 (base, vl);
    asm volatile("#" ::
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", 
		   "v18", "v19", "v20", "v21", "v22", "v23");

    vint8m1_t v = __riscv_vluxei64_v_i8m1(base,bindex,vl);
    asm volatile("#" ::                                                        
		 : "v0", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", 
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17",     
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",     
		   "v26", "v27", "v28", "v29", "v30", "v31");
    v = __riscv_vadd_vv_i8m1 (v,v,vl);
    asm volatile("#" ::                                                        
		 : "v0", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", 
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17",     
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",     
		   "v26", "v27", "v28", "v29", "v30", "v31");

    __riscv_vse8_v_i8m1 (out,v,vl);
}

/* { dg-final { scan-assembler-times {vmv} 1 } } */
/* { dg-final { scan-assembler-not {csrr} } } */

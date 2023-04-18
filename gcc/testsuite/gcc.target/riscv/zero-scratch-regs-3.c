/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O2 -fzero-call-used-regs=all" } */

void
foo (void)
{
}

/* { dg-final { scan-assembler "vsetvli\[ \t\]*t0,zero,e32,m1,tu,mu" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v0,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v1,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v2,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v3,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v4,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v5,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v6,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v7,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v8,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v9,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v10,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v11,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v12,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v13,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v14,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v15,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v16,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v17,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v18,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v19,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v20,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v21,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v22,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v23,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v24,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v25,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v26,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v27,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v28,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v29,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v30,0" } } */
/* { dg-final { scan-assembler "vmv.v.i\[ \t\]*v31,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*t0,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*t1,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*t2,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*a0,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*a1,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*a2,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*a3,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*a4,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*a5,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*a6,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*a7,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*t3,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*t4,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*t5,0" } } */
/* { dg-final { scan-assembler "li\[ \t\]*t6,0" } } */
/* { dg-final { scan-assembler "fmv.d.x\[ \t\]*ft0,zero" } } */

/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -O3 -fgimple" } */

#include "riscv_vector.h"

void __GIMPLE (ssa,guessed_local(1073741824))
f1 (void * out)
{
  __BB(2,guessed_local(1073741824)):
  __MEM <vbool1_t> ((vbool1_t *)out_2(D)) = _Literal (vbool1_t) 0;
  return;

}


void __GIMPLE (ssa,guessed_local(1073741824))
f2 (void * out)
{
  __BB(2,guessed_local(1073741824)):
  __MEM <vbool2_t> ((vbool2_t *)out_2(D)) = _Literal (vbool2_t) 0;
  return;

}


void __GIMPLE (ssa,guessed_local(1073741824))
f3 (void * out)
{
  __BB(2,guessed_local(1073741824)):
  __MEM <vbool4_t> ((vbool4_t *)out_2(D)) = _Literal (vbool4_t) 0;
  return;

}


void __GIMPLE (ssa,guessed_local(1073741824))
f4 (void * out)
{
  __BB(2,guessed_local(1073741824)):
  __MEM <vbool8_t> ((vbool8_t *)out_2(D)) = _Literal (vbool8_t) 0;
  return;

}


void __GIMPLE (ssa,guessed_local(1073741824))
f5 (void * out)
{
  __BB(2,guessed_local(1073741824)):
  __MEM <vbool16_t> ((vbool16_t *)out_2(D)) = _Literal (vbool16_t) 0;
  return;

}


void __GIMPLE (ssa,guessed_local(1073741824))
f6 (void * out)
{
  __BB(2,guessed_local(1073741824)):
  __MEM <vbool32_t> ((vbool32_t *)out_2(D)) = _Literal (vbool32_t) 0;
  return;

}


void __GIMPLE (ssa,guessed_local(1073741824))
f7 (void * out)
{
  __BB(2,guessed_local(1073741824)):
  __MEM <vbool64_t> ((vbool64_t *)out_2(D)) = _Literal (vbool64_t) 0;
  return;

}

/* { dg-final { scan-assembler-times {vmclr\.m\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1])} 7 } } */

/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl" } */

/* { dg-final { scan-assembler-times "vaddph" 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vsubph" 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vmulph" 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vdivph" 2 { target { ! ia32 } } } } */

#define DO_PRAGMA(X) _Pragma(#X)

#define VEC_OP_VV(size, op, name)       \
void \
__attribute__ ((noinline, noclone, optimize("tree-slp-vectorize"))) \
vecop_v##size##hf##name (_Float16 * restrict dst,  \
 _Float16 * restrict src1, _Float16 * restrict src2)   \
{ \
    int i;  \
    DO_PRAGMA (GCC unroll size)   \
    for (i = 0; i < size; i++)  \
      dst[i] = src1[i] op src2[i];  \
}

VEC_OP_VV(4, +, add)
VEC_OP_VV(2, +, add)
VEC_OP_VV(4, -, sub)
VEC_OP_VV(2, -, sub)
VEC_OP_VV(4, *, mul)
VEC_OP_VV(2, *, mul)
VEC_OP_VV(4, /, div)
VEC_OP_VV(2, /, div)

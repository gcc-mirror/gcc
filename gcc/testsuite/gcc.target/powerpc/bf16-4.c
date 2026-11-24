/* { dg-require-effective-target power10_ok } */
/* { dg-require-effective-target ppc_bf16_runtime } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -mfloat16" } */

/* Check that we can do multiple __bf16 adds all in V4SFmode.  */

/* Expected code:

   lxsihzx 0,0,4
   lxsihzx 12,0,5
   xxspltiw 11,1065353216
   xxspltw 0,0,1
   xxspltw 12,12,1
   xvcvbf16spn 0,0
   xvcvbf16spn 12,12
   xvaddsp 0,0,12
   xvaddsp 0,0,11
   xvcvspbf16 0,0
   stxsihx 0,0,3  */

void
add3 (__bf16 *p, __bf16 *q, __bf16 *r)
{
  *p = *q + *r + (__bf16)1.0;
}

/* { dg-final { scan-assembler-times {\mxxspltw\M|\mvspltw\M|\mmtvsrws\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxvcvbf16spn\M}                    2 } } */
/* { dg-final { scan-assembler-times {\mxvaddsp\M}                        2 } } */
/* { dg-final { scan-assembler-times {\mxxspltiw\M}                       1 } } */
/* { dg-final { scan-assembler-times {\mxvcvspbf16\M}                     1 } } */

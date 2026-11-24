/* { dg-require-effective-target power10_ok } */
/* { dg-require-effective-target ppc_bf16_runtime } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -mfloat16" } */

/* Check that we can do multiple __bf16 adds all in V4SFmode.  */

/* Expected code:

   lxsihzx 0,0,4
   lxsihzx 11,0,5
   lxsihzx 12,0,6
   xxspltw 0,0,1
   xxspltw 11,11,1
   xxspltw 12,12,1
   xvcvbf16spn 0,0
   xvcvbf16spn 11,11
   xvcvbf16spn 12,12
   xvaddsp 0,0,11
   xvaddsp 0,0,12
   xvcvspbf16 0,0
   stxsihx 0,0,3  */

void
add3 (__bf16 *p, __bf16 *q, __bf16 *r, __bf16 *s)
{
  *p = *q + *r + *s;
}

/* { dg-final { scan-assembler-times {\mxxspltw\M|\mvspltw\M|\mmtvsrws\M} 3 } } */
/* { dg-final { scan-assembler-times {\mxvcvbf16spn\M}                    3 } } */
/* { dg-final { scan-assembler-times {\mxvaddsp\M}                        2 } } */
/* { dg-final { scan-assembler-times {\mxvcvspbf16\M}                     1 } } */

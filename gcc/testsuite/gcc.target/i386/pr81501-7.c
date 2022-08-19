/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -fplt -mtls-dialect=gnu" } */

extern int __bid_IDEC_glbround, __bid64qqq_fma_save_fpsf;
extern __thread int __bid_IDEC_glbflags;
typedef struct {
  long w[2];
} UINT128;
extern long __bid64qqq_fma_res_0_1;
extern void bid128_ext_fma(UINT128, UINT128);
void
__bid64qqq_fma(UINT128 y, UINT128 z)
{
  __bid_IDEC_glbflags = 0;
  bid128_ext_fma(y, z);
  if (__bid_IDEC_glbround || __bid64qqq_fma_res_0_1)
    __bid_IDEC_glbflags |= __bid64qqq_fma_save_fpsf;
}

/* { dg-final { scan-assembler-times "call\[ \t\]__tls_get_addr@PLT" 1 { target { ! ia32 } } } } */

/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-mcpu=power7 -mtune=power9 -O3 -msoft-float -dp" } */

#define LARGE 0x12345

float fusion_float_read (float *p){ return p[LARGE]; }

void fusion_float_write (float *p, float f){ p[LARGE] = f; }

/* { dg-final { scan-assembler {fusion_gpr_[sd]i_sf_store}  } } */

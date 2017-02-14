/* { dg-do compile { target { powerpc*-*-* && ilp32 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-mcpu=power7 -mtune=power9 -O3 -msoft-float" } */

#define LARGE 0x12345

float fusion_float_read (float *p){ return p[LARGE]; }

void fusion_float_write (float *p, float f){ p[LARGE] = f; }

/* { dg-final { scan-assembler "store fusion, type SF" } } */

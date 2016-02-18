/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-mcpu=power7 -mtune=power9 -O3" } */

#define LARGE 0x12345

int fusion_float_read (float *p){ return p[LARGE]; }
int fusion_double_read (double *p){ return p[LARGE]; }

void fusion_float_write (float *p, float f){ p[LARGE] = f; }
void fusion_double_write (double *p, double d){ p[LARGE] = d; }

/* { dg-final { scan-assembler "load fusion, type SF"  } } */
/* { dg-final { scan-assembler "load fusion, type DF"  } } */
/* { dg-final { scan-assembler "store fusion, type SF" } } */
/* { dg-final { scan-assembler "store fusion, type DF" } } */

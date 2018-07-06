/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-mcpu=power7 -mtune=power9 -O3 -dp" } */

#define LARGE 0x12345

int fusion_float_read (float *p){ return p[LARGE]; }
int fusion_double_read (double *p){ return p[LARGE]; }

void fusion_float_write (float *p, float f){ p[LARGE] = f; }
void fusion_double_write (double *p, double d){ p[LARGE] = d; }

/* { dg-final { scan-assembler {fusion_vsx_[sd]i_sf_load}  } } */
/* { dg-final { scan-assembler {fusion_vsx_[sd]i_df_load}  } } */
/* { dg-final { scan-assembler {fusion_vsx_[sd]i_sf_store}  } } */
/* { dg-final { scan-assembler {fusion_vsx_[sd]i_df_store}  } } */

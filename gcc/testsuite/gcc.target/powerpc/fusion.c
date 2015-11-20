/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-mcpu=power7 -mtune=power8 -O3" } */

#define LARGE 0x12345

int fusion_uchar (unsigned char *p){ return p[LARGE]; }
int fusion_schar (signed char *p){ return p[LARGE]; }
int fusion_ushort (unsigned short *p){ return p[LARGE]; }
int fusion_short (short *p){ return p[LARGE]; }
int fusion_int (int *p){ return p[LARGE]; }
unsigned fusion_uns (unsigned *p){ return p[LARGE]; }

/* { dg-final { scan-assembler-times "gpr load fusion"    6 } } */
/* { dg-final { scan-assembler-times "lbz"                2 } } */
/* { dg-final { scan-assembler-times "extsb"              1 } } */
/* { dg-final { scan-assembler-times "lhz"                2 } } */
/* { dg-final { scan-assembler-times "extsh"              1 } } */
/* { dg-final { scan-assembler-times "lwz"                2 } } */

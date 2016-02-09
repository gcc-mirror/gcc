/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-mcpu=power7 -mtune=power9 -O3" } */

#define SIZE 4
struct foo {
  float f;
  double d;
};

static struct foo st[SIZE];
struct foo *ptr_st = &st[0];

float fusion_float_read (void){ return st[SIZE].f; }
double fusion_float_extend (void){ return (double)st[SIZE].f; }
double fusion_double_read (void){ return st[SIZE].d; }

void fusion_float_write (float f){ st[SIZE].f = f; }
void fusion_float_truncate (double d){ st[SIZE].f = (float)d; }
void fusion_double_write (double d){ st[SIZE].d = d; }

/* { dg-final { scan-assembler-times "load fusion, type SF"  2 } } */
/* { dg-final { scan-assembler-times "load fusion, type DF"  1 } } */
/* { dg-final { scan-assembler-times "store fusion, type SF" 2 } } */
/* { dg-final { scan-assembler-times "store fusion, type DF" 1 } } */

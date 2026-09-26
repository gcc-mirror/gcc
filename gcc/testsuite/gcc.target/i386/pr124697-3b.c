/* { dg-do compile }  */
/* { dg-options "-O2 -mavx512f -mtune=generic" } */
/* { dg-final { scan-assembler-times "vmovupd.*, %zmm0" 1 } } */
/* { dg-final { scan-assembler "and\[lq\]?\[\\t \]*\\$-64,\[\\t \]*%\[re\]?sp" } } */
/* { dg-final { scan-assembler-not "vmovapd.*, %zmm0" } } */

typedef double v4df __attribute__((vector_size(32)));
typedef double v8df __attribute__((vector_size(64)));
typedef struct {
  v4df a[2];
} c __attribute__((aligned(64)));
v8df d;
c v;

void
foo (float a1, float a2, float a3, float a4, float a5, float a6, c f)
{
  d = *(v8df *) &f;
}

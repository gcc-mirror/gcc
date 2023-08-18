/* PR target/111023 */
/* { dg-do compile } */
/* { dg-options "-O2 -mtune=icelake-server -ftree-vectorize -msse2 -mno-sse4.1" } */

typedef char v16qi __attribute__((vector_size (16)));
typedef short v8hi __attribute__((vector_size (16)));
typedef int v4si __attribute__((vector_size (16)));
typedef long long v2di __attribute__((vector_size (16)));

void
v8hi_v8qi (v8hi *dst, v16qi src)
{
  short tem[8];
  tem[0] = src[0];
  tem[1] = src[1];
  tem[2] = src[2];
  tem[3] = src[3];
  tem[4] = src[4];
  tem[5] = src[5];
  tem[6] = src[6];
  tem[7] = src[7];
  dst[0] = *(v8hi *) tem;
}

/* { dg-final { scan-assembler "pcmpgtb" } } */
/* { dg-final { scan-assembler "punpcklbw" } } */

void
v4si_v4hi (v4si *dst, v8hi src)
{
  int tem[4];
  tem[0] = src[0];
  tem[1] = src[1];
  tem[2] = src[2];
  tem[3] = src[3];
  dst[0] = *(v4si *) tem;
}

/* { dg-final { scan-assembler "pcmpgtw" } } */
/* { dg-final { scan-assembler "punpcklwd" } } */

void
v2di_v2si (v2di *dst, v4si src)
{
  long long tem[2];
  tem[0] = src[0];
  tem[1] = src[1];
  dst[0] = *(v2di *) tem;
}

/* { dg-final { scan-assembler "pcmpgtd" } } */
/* { dg-final { scan-assembler "punpckldq" } } */

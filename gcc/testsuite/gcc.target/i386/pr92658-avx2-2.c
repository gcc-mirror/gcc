/* PR target/92658 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mavx2" } */

typedef char v32qi __attribute__((vector_size (32)));
typedef short v16hi __attribute__((vector_size (32)));
typedef int v8si __attribute__((vector_size (32)));
typedef long long v4di __attribute__((vector_size (32)));

void
foo_s8_s16 (v16hi * dst, v32qi * __restrict src)
{
  short tem[16];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  tem[4] = (*src)[4];
  tem[5] = (*src)[5];
  tem[6] = (*src)[6];
  tem[7] = (*src)[7];
  tem[8] = (*src)[8];
  tem[9] = (*src)[9];
  tem[10] = (*src)[10];
  tem[11] = (*src)[11];
  tem[12] = (*src)[12];
  tem[13] = (*src)[13];
  tem[14] = (*src)[14];
  tem[15] = (*src)[15];
  dst[0] = *(v16hi *) tem;
}

void
bar_s8_s16 (v16hi * dst, v32qi src)
{
  short tem[16];
  tem[0] = src[0];
  tem[1] = src[1];
  tem[2] = src[2];
  tem[3] = src[3];
  tem[4] = src[4];
  tem[5] = src[5];
  tem[6] = src[6];
  tem[7] = src[7];
  tem[8] = src[8];
  tem[9] = src[9];
  tem[10] = src[10];
  tem[11] = src[11];
  tem[12] = src[12];
  tem[13] = src[13];
  tem[14] = src[14];
  tem[15] = src[15];
  dst[0] = *(v16hi *) tem;
}

/* { dg-final { scan-assembler-times "pmovsxbw" 2 } } */

void
foo_s8_s32 (v8si * dst, v32qi * __restrict src)
{
  int tem[8];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  tem[4] = (*src)[4];
  tem[5] = (*src)[5];
  tem[6] = (*src)[6];
  tem[7] = (*src)[7];
  dst[0] = *(v8si *) tem;
}

void
bar_s8_s32 (v8si * dst, v32qi src)
{
  int tem[8];
  tem[0] = src[0];
  tem[1] = src[1];
  tem[2] = src[2];
  tem[3] = src[3];
  tem[4] = src[4];
  tem[5] = src[5];
  tem[6] = src[6];
  tem[7] = src[7];
  dst[0] = *(v8si *) tem;
}

/* { dg-final { scan-assembler-times "pmovsxbd" 2 } } */

void
foo_s8_s64 (v4di * dst, v32qi * __restrict src)
{
  long long tem[4];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  dst[0] = *(v4di *) tem;
}

void
bar_s8_s64 (v4di * dst, v32qi src)
{
  long long tem[4];
  tem[0] = src[0];
  tem[1] = src[1];
  tem[2] = src[2];
  tem[3] = src[3];
  dst[0] = *(v4di *) tem;
}

/* { dg-final { scan-assembler-times "pmovsxbq" 2 } } */

void
foo_s16_s32 (v8si * dst, v16hi * __restrict src)
{
  int tem[8];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  tem[4] = (*src)[4];
  tem[5] = (*src)[5];
  tem[6] = (*src)[6];
  tem[7] = (*src)[7];
  dst[0] = *(v8si *) tem;
}

void
bar_s16_s32 (v8si * dst, v16hi src)
{
  int tem[8];
  tem[0] = src[0];
  tem[1] = src[1];
  tem[2] = src[2];
  tem[3] = src[3];
  tem[4] = src[4];
  tem[5] = src[5];
  tem[6] = src[6];
  tem[7] = src[7];
  dst[0] = *(v8si *) tem;
}

/* { dg-final { scan-assembler-times "pmovsxwd" 2 } } */

void
foo_s16_s64 (v4di * dst, v16hi * __restrict src)
{
  long long tem[4];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  dst[0] = *(v4di *) tem;
}

void
bar_s16_s64 (v4di * dst, v16hi src)
{
  long long tem[4];
  tem[0] = src[0];
  tem[1] = src[1];
  tem[2] = src[2];
  tem[3] = src[3];
  dst[0] = *(v4di *) tem;
}

/* { dg-final { scan-assembler-times "pmovsxwq" 2 } } */

void
foo_s32_s64 (v4di * dst, v8si * __restrict src)
{
  long long tem[4];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  dst[0] = *(v4di *) tem;
}

void
bar_s32_s64 (v4di * dst, v8si src)
{
  long long tem[4];
  tem[0] = src[0];
  tem[1] = src[1];
  tem[2] = src[2];
  tem[3] = src[3];
  dst[0] = *(v4di *) tem;
}

/* { dg-final { scan-assembler-times "pmovsxdq" 2 } } */

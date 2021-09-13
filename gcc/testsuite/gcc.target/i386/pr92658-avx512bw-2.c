/* PR target/92658 */
/* { dg-do compile } */
/* { dg-options "-O2 -mtune=icelake-server -ftree-vectorize -mavx512bw -mprefer-vector-width=512" } */

typedef char v64qi __attribute__((vector_size (64)));
typedef short v32hi __attribute__((vector_size (64)));
typedef int v16si __attribute__((vector_size (64)));
typedef long long v8di __attribute__((vector_size (64)));

void
foo_s8_s16 (v32hi * dst, v64qi * __restrict src)
{
  short tem[32];
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
  tem[16] = (*src)[16];
  tem[17] = (*src)[17];
  tem[18] = (*src)[18];
  tem[19] = (*src)[19];
  tem[20] = (*src)[20];
  tem[21] = (*src)[21];
  tem[22] = (*src)[22];
  tem[23] = (*src)[23];
  tem[24] = (*src)[24];
  tem[25] = (*src)[25];
  tem[26] = (*src)[26];
  tem[27] = (*src)[27];
  tem[28] = (*src)[28];
  tem[29] = (*src)[29];
  tem[30] = (*src)[30];
  tem[31] = (*src)[31];
  dst[0] = *(v32hi *) tem;
}

void
bar_s8_s16 (v32hi * dst, v64qi src)
{
  short tem[32];
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
  tem[16] = src[16];
  tem[17] = src[17];
  tem[18] = src[18];
  tem[19] = src[19];
  tem[20] = src[20];
  tem[21] = src[21];
  tem[22] = src[22];
  tem[23] = src[23];
  tem[24] = src[24];
  tem[25] = src[25];
  tem[26] = src[26];
  tem[27] = src[27];
  tem[28] = src[28];
  tem[29] = src[29];
  tem[30] = src[30];
  tem[31] = src[31];
  dst[0] = *(v32hi *) tem;
}

/* { dg-final { scan-assembler-times "pmovsxbw" 2 } } */

void
foo_s8_s32 (v16si * dst, v64qi * __restrict src)
{
  int tem[16];
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
  dst[0] = *(v16si *) tem;
}

void
bar_s8_s32 (v16si * dst, v64qi src)
{
  int tem[16];
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
  dst[0] = *(v16si *) tem;
}

/* { dg-final { scan-assembler-times "pmovsxbd" 2 } } */

void
foo_s8_s64 (v8di * dst, v64qi * __restrict src)
{
  long long tem[8];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  tem[4] = (*src)[4];
  tem[5] = (*src)[5];
  tem[6] = (*src)[6];
  tem[7] = (*src)[7];
  dst[0] = *(v8di *) tem;
}

void
bar_s8_s64 (v8di * dst, v64qi src)
{
  long long tem[8];
  tem[0] = src[0];
  tem[1] = src[1];
  tem[2] = src[2];
  tem[3] = src[3];
  tem[4] = src[4];
  tem[5] = src[5];
  tem[6] = src[6];
  tem[7] = src[7];
  dst[0] = *(v8di *) tem;
}

/* { dg-final { scan-assembler-times "pmovsxbq" 2 } } */

void
foo_s16_s32 (v16si * dst, v32hi * __restrict src)
{
  int tem[16];
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
  dst[0] = *(v16si *) tem;
}

void
bar_s16_s32 (v16si * dst, v32hi src)
{
  int tem[16];
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
  dst[0] = *(v16si *) tem;
}

/* { dg-final { scan-assembler-times "pmovsxwd" 2 } } */

void
foo_s16_s64 (v8di * dst, v32hi * __restrict src)
{
  long long tem[8];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  tem[4] = (*src)[4];
  tem[5] = (*src)[5];
  tem[6] = (*src)[6];
  tem[7] = (*src)[7];
  dst[0] = *(v8di *) tem;
}

void
bar_s16_s64 (v8di * dst, v32hi src)
{
  long long tem[8];
  tem[0] = src[0];
  tem[1] = src[1];
  tem[2] = src[2];
  tem[3] = src[3];
  tem[4] = src[4];
  tem[5] = src[5];
  tem[6] = src[6];
  tem[7] = src[7];
  dst[0] = *(v8di *) tem;
}

/* { dg-final { scan-assembler-times "pmovsxwq" 2 } } */

void
foo_s32_s64 (v8di * dst, v16si * __restrict src)
{
  long long tem[8];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  tem[4] = (*src)[4];
  tem[5] = (*src)[5];
  tem[6] = (*src)[6];
  tem[7] = (*src)[7];
  dst[0] = *(v8di *) tem;
}

void
bar_s32_s64 (v8di * dst, v16si src)
{
  long long tem[8];
  tem[0] = src[0];
  tem[1] = src[1];
  tem[2] = src[2];
  tem[3] = src[3];
  tem[4] = src[4];
  tem[5] = src[5];
  tem[6] = src[6];
  tem[7] = src[7];
  dst[0] = *(v8di *) tem;
}

/* { dg-final { scan-assembler-times "pmovsxdq" 2 } } */

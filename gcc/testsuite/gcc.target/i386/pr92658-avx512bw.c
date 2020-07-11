/* PR target/92658 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mavx512bw" } */

typedef unsigned char v64qi __attribute__((vector_size (64)));
typedef unsigned short v32hi __attribute__((vector_size (64)));
typedef unsigned int v16si __attribute__((vector_size (64)));
typedef unsigned long long v8di __attribute__((vector_size (64)));

void
foo_u8_u16 (v32hi * dst, v64qi * __restrict src)
{
  unsigned short tem[32];
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
bar_u8_u16 (v32hi * dst, v64qi src)
{
  unsigned short tem[32];
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

/* { dg-final { scan-assembler-times "pmovzxbw" 2 } } */

void
foo_u8_u32 (v16si * dst, v64qi * __restrict src)
{
  unsigned int tem[16];
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
bar_u8_u32 (v16si * dst, v64qi src)
{
  unsigned int tem[16];
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

/* { dg-final { scan-assembler-times "pmovzxbd" 2 } } */

void
foo_u8_u64 (v8di * dst, v64qi * __restrict src)
{
  unsigned long long tem[8];
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
bar_u8_u64 (v8di * dst, v64qi src)
{
  unsigned long long tem[8];
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

/* { dg-final { scan-assembler-times "pmovzxbq" 2 } } */

void
foo_u16_u32 (v16si * dst, v32hi * __restrict src)
{
  unsigned int tem[16];
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
bar_u16_u32 (v16si * dst, v32hi src)
{
  unsigned int tem[16];
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

/* { dg-final { scan-assembler-times "pmovzxwd" 2 } } */

void
foo_u16_u64 (v8di * dst, v32hi * __restrict src)
{
  unsigned long long tem[8];
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
bar_u16_u64 (v8di * dst, v32hi src)
{
  unsigned long long tem[8];
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

/* { dg-final { scan-assembler-times "pmovzxwq" 2 } } */

void
foo_u32_u64 (v8di * dst, v16si * __restrict src)
{
  unsigned long long tem[8];
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
bar_u32_u64 (v8di * dst, v16si src)
{
  unsigned long long tem[8];
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

/* { dg-final { scan-assembler-times "pmovzxdq" 2 } } */

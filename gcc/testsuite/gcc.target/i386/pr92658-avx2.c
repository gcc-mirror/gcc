/* PR target/92658 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mavx2" } */

typedef unsigned char v32qi __attribute__((vector_size (32)));
typedef unsigned short v16hi __attribute__((vector_size (32)));
typedef unsigned int v8si __attribute__((vector_size (32)));
typedef unsigned long long v4di __attribute__((vector_size (32)));

void
foo_u8_u16 (v16hi * dst, v32qi * __restrict src)
{
  unsigned short tem[16];
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
bar_u8_u16 (v16hi * dst, v32qi src)
{
  unsigned short tem[16];
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

/* { dg-final { scan-assembler-times "pmovzxbw" 2 } } */

void
foo_u8_u32 (v8si * dst, v32qi * __restrict src)
{
  unsigned int tem[8];
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
bar_u8_u32 (v8si * dst, v32qi src)
{
  unsigned int tem[8];
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

/* { dg-final { scan-assembler-times "pmovzxbd" 2 } } */

void
foo_u8_u64 (v4di * dst, v32qi * __restrict src)
{
  unsigned long long tem[4];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  dst[0] = *(v4di *) tem;
}

void
bar_u8_u64 (v4di * dst, v32qi src)
{
  unsigned long long tem[4];
  tem[0] = src[0];
  tem[1] = src[1];
  tem[2] = src[2];
  tem[3] = src[3];
  dst[0] = *(v4di *) tem;
}

/* { dg-final { scan-assembler-times "pmovzxbq" 2 } } */

void
foo_u16_u32 (v8si * dst, v16hi * __restrict src)
{
  unsigned int tem[8];
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
bar_u16_u32 (v8si * dst, v16hi src)
{
  unsigned int tem[8];
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

/* { dg-final { scan-assembler-times "pmovzxwd" 2 } } */

void
foo_u16_u64 (v4di * dst, v16hi * __restrict src)
{
  unsigned long long tem[4];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  dst[0] = *(v4di *) tem;
}

void
bar_u16_u64 (v4di * dst, v16hi src)
{
  unsigned long long tem[4];
  tem[0] = src[0];
  tem[1] = src[1];
  tem[2] = src[2];
  tem[3] = src[3];
  dst[0] = *(v4di *) tem;
}

/* { dg-final { scan-assembler-times "pmovzxwq" 2 } } */

void
foo_u32_u64 (v4di * dst, v8si * __restrict src)
{
  unsigned long long tem[4];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  dst[0] = *(v4di *) tem;
}

void
bar_u32_u64 (v4di * dst, v8si src)
{
  unsigned long long tem[4];
  tem[0] = src[0];
  tem[1] = src[1];
  tem[2] = src[2];
  tem[3] = src[3];
  dst[0] = *(v4di *) tem;
}

/* { dg-final { scan-assembler-times "pmovzxdq" 2 } } */

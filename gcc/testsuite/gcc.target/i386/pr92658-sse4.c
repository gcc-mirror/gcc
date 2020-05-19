/* PR target/92658 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msse4.1" } */

typedef unsigned char v16qi __attribute__((vector_size (16)));
typedef unsigned short v8hi __attribute__((vector_size (16)));
typedef unsigned int v4si __attribute__((vector_size (16)));
typedef unsigned long long v2di __attribute__((vector_size (16)));

void
foo_u8_u16 (v8hi * dst, v16qi * __restrict src)
{
  unsigned short tem[8];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  tem[4] = (*src)[4];
  tem[5] = (*src)[5];
  tem[6] = (*src)[6];
  tem[7] = (*src)[7];
  dst[0] = *(v8hi *) tem;
}

void
bar_u8_u16 (v8hi * dst, v16qi src)
{
  unsigned short tem[8];
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

/* { dg-final { scan-assembler-times "pmovzxbw" 2 } } */

void
foo_u8_u32 (v4si * dst, v16qi * __restrict src)
{
  unsigned int tem[4];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  dst[0] = *(v4si *) tem;
}

void
bar_u8_u32 (v4si * dst, v16qi src)
{
  unsigned int tem[4];
  tem[0] = src[0];
  tem[1] = src[1];
  tem[2] = src[2];
  tem[3] = src[3];
  dst[0] = *(v4si *) tem;
}

/* { dg-final { scan-assembler-times "pmovzxbd" 2 { xfail *-*-* } } } */

void
foo_u8_u64 (v2di * dst, v16qi * __restrict src)
{
  unsigned long long tem[2];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  dst[0] = *(v2di *) tem;
}

void
bar_u8_u64 (v2di * dst, v16qi src)
{
  unsigned long long tem[2];
  tem[0] = src[0];
  tem[1] = src[1];
  dst[0] = *(v2di *) tem;
}

/* { dg-final { scan-assembler-times "pmovzxbq" 2 { xfail *-*-* } } } */

void
foo_u16_u32 (v4si * dst, v8hi * __restrict src)
{
  unsigned int tem[4];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  dst[0] = *(v4si *) tem;
}

void
bar_u16_u32 (v4si * dst, v8hi src)
{
  unsigned int tem[4];
  tem[0] = src[0];
  tem[1] = src[1];
  tem[2] = src[2];
  tem[3] = src[3];
  dst[0] = *(v4si *) tem;
}

/* { dg-final { scan-assembler-times "pmovzxwd" 2 } } */

void
foo_u16_u64 (v2di * dst, v8hi * __restrict src)
{
  unsigned long long tem[2];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  dst[0] = *(v2di *) tem;
}

void
bar_u16_u64 (v2di * dst, v8hi src)
{
  unsigned long long tem[2];
  tem[0] = src[0];
  tem[1] = src[1];
  dst[0] = *(v2di *) tem;
}

/* { dg-final { scan-assembler-times "pmovzxwq" 2 { xfail *-*-* } } } */

void
foo_u32_u64 (v2di * dst, v4si * __restrict src)
{
  unsigned long long tem[2];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  dst[0] = *(v2di *) tem;
}

void
bar_u32_u64 (v2di * dst, v4si src)
{
  unsigned long long tem[2];
  tem[0] = src[0];
  tem[1] = src[1];
  dst[0] = *(v2di *) tem;
}

/* { dg-final { scan-assembler-times "pmovzxdq" 2 } } */

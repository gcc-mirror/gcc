/* PR target/92658 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mtune=icelake-server -ftree-vectorize -msse4.1" } */

typedef unsigned char v8qi __attribute__((vector_size (8)));
typedef unsigned short v4hi __attribute__((vector_size (8)));
typedef unsigned int v2si __attribute__((vector_size (8)));

void
foo_u8_u16 (v4hi * dst, v8qi * __restrict src)
{
  unsigned short tem[4];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  dst[0] = *(v4hi *) tem;
}

void
bar_u8_u16 (v4hi * dst, v8qi src)
{
  unsigned short tem[4];
  tem[0] = src[0];
  tem[1] = src[1];
  tem[2] = src[2];
  tem[3] = src[3];
  dst[0] = *(v4hi *) tem;
}

/* { dg-final { scan-assembler-times "pmovzxbw" 2 } } */

void
foo_u8_u32 (v2si * dst, v8qi * __restrict src)
{
  unsigned int tem[2];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  dst[0] = *(v2si *) tem;
}

void
bar_u8_u32 (v2si * dst, v8qi src)
{
  unsigned int tem[2];
  tem[0] = src[0];
  tem[1] = src[1];
  dst[0] = *(v2si *) tem;
}

/* { dg-final { scan-assembler-times "pmovzxbd" 2 } } */

void
foo_u16_u32 (v2si * dst, v4hi * __restrict src)
{
  unsigned int tem[2];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  dst[0] = *(v2si *) tem;
}

void
bar_u16_u32 (v2si * dst, v4hi src)
{
  unsigned int tem[2];
  tem[0] = src[0];
  tem[1] = src[1];
  dst[0] = *(v2si *) tem;
}

/* { dg-final { scan-assembler-times "pmovzxwd" 2 } } */

/* PR target/111023 */
/* { dg-do compile  { target { ! ia32 } } } */
/* { dg-options "-O2 -mtune=icelake-server -ftree-vectorize -msse2 -mno-sse4.1" } */

typedef unsigned char v8qi __attribute__((vector_size (8)));
typedef unsigned short v4hi __attribute__((vector_size (8)));
typedef unsigned int v2si __attribute__((vector_size (8)));

void
v4hi_v4qi (v4hi *dst, v8qi src)
{
  unsigned short tem[4];
  tem[0] = src[0];
  tem[1] = src[1];
  tem[2] = src[2];
  tem[3] = src[3];
  dst[0] = *(v4hi *) tem;
}

/* { dg-final { scan-assembler "punpcklbw" } } */

void
v2si_v2hi (v2si *dst, v4hi src)
{
  unsigned int tem[2];
  tem[0] = src[0];
  tem[1] = src[1];
  dst[0] = *(v2si *) tem;
}

/* { dg-final { scan-assembler "punpcklwd" } } */

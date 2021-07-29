/* PR target/92658 */
/* { dg-do compile } */
/* { dg-options "-O2 -mtune=icelake-server -ftree-vectorize -msse4.1" } */

typedef char v16qi __attribute__((vector_size (16)));
typedef short v8hi __attribute__((vector_size (16)));
typedef int v4si __attribute__((vector_size (16)));
typedef long long v2di __attribute__((vector_size (16)));

void
foo_s8_s16 (v8hi * dst, v16qi * __restrict src)
{
  short tem[8];
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
bar_s8_s16 (v8hi * dst, v16qi src)
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

/* { dg-final { scan-assembler-times "pmovsxbw" 2 } } */

void
foo_s8_s32 (v4si * dst, v16qi * __restrict src)
{
  int tem[4];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  dst[0] = *(v4si *) tem;
}

void
bar_s8_s32 (v4si * dst, v16qi src)
{
  int tem[4];
  tem[0] = src[0];
  tem[1] = src[1];
  tem[2] = src[2];
  tem[3] = src[3];
  dst[0] = *(v4si *) tem;
}

/* { dg-final { scan-assembler-times "pmovsxbd" 2 } } */

void
foo_s8_s64 (v2di * dst, v16qi * __restrict src)
{
  long long tem[2];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  dst[0] = *(v2di *) tem;
}

void
bar_s8_s64 (v2di * dst, v16qi src)
{
  long long tem[2];
  tem[0] = src[0];
  tem[1] = src[1];
  dst[0] = *(v2di *) tem;
}

/* { dg-final { scan-assembler-times "pmovsxbq" 2 { xfail *-*-* } } } */

void
foo_s16_s32 (v4si * dst, v8hi * __restrict src)
{
  int tem[4];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  dst[0] = *(v4si *) tem;
}

void
bar_s16_s32 (v4si * dst, v8hi src)
{
  int tem[4];
  tem[0] = src[0];
  tem[1] = src[1];
  tem[2] = src[2];
  tem[3] = src[3];
  dst[0] = *(v4si *) tem;
}

/* { dg-final { scan-assembler-times "pmovsxwd" 2 } } */

void
foo_s16_s64 (v2di * dst, v8hi * __restrict src)
{
  long long tem[2];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  dst[0] = *(v2di *) tem;
}

void
bar_s16_s64 (v2di * dst, v8hi src)
{
  long long tem[2];
  tem[0] = src[0];
  tem[1] = src[1];
  dst[0] = *(v2di *) tem;
}

/* { dg-final { scan-assembler-times "pmovsxwq" 2 } } */

void
foo_s32_s64 (v2di * dst, v4si * __restrict src)
{
  long long tem[2];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  dst[0] = *(v2di *) tem;
}

void
bar_s32_s64 (v2di * dst, v4si src)
{
  long long tem[2];
  tem[0] = src[0];
  tem[1] = src[1];
  dst[0] = *(v2di *) tem;
}

/* { dg-final { scan-assembler-times "pmovsxdq" 2 } } */

/* PR target/92658 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mavx512f -mavx512vl" } */

typedef unsigned char v16qi __attribute__((vector_size (16)));
typedef unsigned short v8hi __attribute__((vector_size (16)));
typedef unsigned int v4si __attribute__((vector_size (16)));
typedef unsigned int v8si __attribute__((vector_size (32)));
typedef unsigned long long v2di __attribute__((vector_size (16)));
typedef unsigned long long v4di __attribute__((vector_size (32)));

void
truncqd_256 (v4si * dst, v4di * __restrict src)
{
  unsigned tem[4];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  dst[0] = *(v4si *) tem;
}

void
truncqw_256 (v8hi * dst, v4di * __restrict src)
{
  unsigned short tem[4];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  dst[0] = *(v8hi *) tem;
}

void
truncqb_256 (v16qi * dst, v4di * __restrict src)
{
  unsigned char tem[4];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  dst[0] = *(v16qi *) tem;
}

void
truncqd_128 (v4si * dst, v2di * __restrict src)
{
  unsigned tem[4];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  dst[0] = *(v4si *) tem;
}

void
truncqw_128 (v8hi * dst, v2di * __restrict src)
{
  unsigned short tem[4];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  dst[0] = *(v8hi *) tem;
}

void
truncqb_128 (v16qi * dst, v2di * __restrict src)
{
  unsigned char tem[4];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  dst[0] = *(v16qi *) tem;
}

void
truncdw_256 (v8hi * dst, v8si * __restrict src)
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
truncdb_256 (v16qi * dst, v8si * __restrict src)
{
  unsigned char tem[8];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  tem[4] = (*src)[4];
  tem[5] = (*src)[5];
  tem[6] = (*src)[6];
  tem[7] = (*src)[7];
  dst[0] = *(v16qi *) tem;
}

void
truncdw_128 (v8hi * dst, v4si * __restrict src)
{
  unsigned short tem[8];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  dst[0] = *(v8hi *) tem;
}

void
truncdb_128 (v16qi * dst, v4si * __restrict src)
{
  unsigned char tem[8];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  dst[0] = *(v16qi *) tem;
}

/* { dg-final { scan-assembler-times "vpmovqd" 2 } } */
/* { dg-final { scan-assembler-times "vpmovqw" 2 } } */
/* { dg-final { scan-assembler-times "vpmovqb\[ \t]*%ymm" 1 } }  */
/* { dg-final { scan-assembler-times "vpmovqb\[ \t]*%xmm" 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times "vpmovdw" 2 } } */
/* { dg-final { scan-assembler-times "vpmovdb" 2 } } */

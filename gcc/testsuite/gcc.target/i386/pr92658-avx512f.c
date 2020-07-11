/* PR target/92658 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mavx512f" } */

typedef unsigned char v8qi __attribute__((vector_size (8)));
typedef unsigned char v16qi __attribute__((vector_size (16)));
typedef unsigned short v8hi __attribute__((vector_size (16)));
typedef unsigned short v16hi __attribute__((vector_size (32)));
typedef unsigned int v8si __attribute__((vector_size (32)));
typedef unsigned int v16si __attribute__((vector_size (64)));
typedef unsigned long long v8di __attribute__((vector_size (64)));

void
truncqd (v8si * dst, v8di * __restrict src)
{
  unsigned tem[8];
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
truncqw (v8hi * dst, v8di * __restrict src)
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
truncqb (v8qi * dst, v8di * __restrict src)
{
  unsigned char tem[8];
  tem[0] = (*src)[0];
  tem[1] = (*src)[1];
  tem[2] = (*src)[2];
  tem[3] = (*src)[3];
  dst[0] = *(v8qi *) tem;
}

void
truncdw (v16hi * dst, v16si * __restrict src)
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
truncdb (v16qi * dst, v16si * __restrict src)
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
  tem[8] = (*src)[8];
  tem[9] = (*src)[9];
  tem[10] = (*src)[10];
  tem[11] = (*src)[11];
  tem[12] = (*src)[12];
  tem[13] = (*src)[13];
  tem[14] = (*src)[14];
  tem[15] = (*src)[15];
  dst[0] = *(v16qi *) tem;
}

/* { dg-final { scan-assembler-times "vpmovqd" 1 } } */
/* { dg-final { scan-assembler-times "vpmovqw" 1 } } */
/* { dg-final { scan-assembler-times "vpmovqb" 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times "vpmovdw" 1 } } */
/* { dg-final { scan-assembler-times "vpmovdb" 1 } } */


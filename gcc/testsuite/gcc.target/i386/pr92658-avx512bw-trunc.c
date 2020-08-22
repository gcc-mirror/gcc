/* PR target/92658 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mavx512bw -mavx512vl" } */

typedef unsigned char v8qi __attribute__((vector_size (8)));
typedef unsigned char v16qi __attribute__((vector_size (16)));
typedef unsigned char v32qi __attribute__((vector_size (32)));
typedef unsigned short v8hi __attribute__((vector_size (16)));
typedef unsigned short v16hi __attribute__((vector_size (32)));
typedef unsigned short v32hi __attribute__((vector_size (64)));


void
truncwb_512 (v32qi * dst, v32hi * __restrict src)
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
  dst[0] = *(v32qi *) tem;
}

void
truncwb_256 (v16qi * dst, v16hi * __restrict src)
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

void
truncwb_128 (v16qi * dst, v8hi * __restrict src)
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

/* { dg-final { scan-assembler-times "vpmovwb" 3 } } */

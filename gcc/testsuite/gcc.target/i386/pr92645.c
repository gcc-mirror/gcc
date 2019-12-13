/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized -msse2 -Wno-psabi" } */

typedef unsigned short v8hi __attribute__((vector_size(16)));
typedef unsigned int v4si __attribute__((vector_size(16)));

void bar (v4si *dst, v8hi * __restrict src)
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
  dst[0] = *(v4si *)tem;
  dst[1] = *(v4si *)&tem[4];
}
void foo (v4si *dst, v8hi src)
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
  dst[0] = *(v4si *)tem;
  dst[1] = *(v4si *)&tem[4];
}

/* { dg-final { scan-tree-dump-times "vec_unpack_" 4 "optimized" } } */

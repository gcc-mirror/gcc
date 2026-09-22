/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-options "-O2 -Wno-psabi -fdump-tree-optimized" } */

typedef unsigned char v8qi __attribute__((vector_size (8)));
typedef unsigned int v4si __attribute__((vector_size (16)));
typedef int s4si __attribute__((vector_size (16)));

/* Both rules want a scalar integral type and an INTEGER_CST, so a vector
   must decline rather than reach get_nonzero_bits (PR127379).  */
v4si
vx (v4si x)
{
  v4si m = { 0x80, 0x80, 0x80, 0x80 };
  return ((x & (v4si) { 0xff, 0xff, 0xff, 0xff }) ^ m) - m;
}

s4si
vxs (s4si x)
{
  s4si m = { 0x80, 0x80, 0x80, 0x80 };
  return ((x & (s4si) { 0xff, 0xff, 0xff, 0xff }) ^ m) - m;
}

v8qi
vq (v8qi x)
{
  v8qi m = { 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40 };
  v8qi l = { 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f };
  return ((x & l) ^ m) - m;
}

v4si
vnu (v4si x)
{
  v4si m = { 0x80, 0x8000, 0x80, 0x80 };
  return (x ^ m) - m;
}

/* { dg-final { scan-tree-dump-times " \\^ " 4 "optimized" } } */

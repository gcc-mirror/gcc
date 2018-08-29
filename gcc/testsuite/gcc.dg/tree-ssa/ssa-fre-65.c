/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1-details -fdump-tree-dse1-details" } */

typedef unsigned char v16qi __attribute__((vector_size(16)));
typedef unsigned short v8hi __attribute__((vector_size(16)));
typedef unsigned int v4si __attribute__((vector_size(16)));
void foo(char *dest)
{
  unsigned char x[256] __attribute__((aligned(16)));
  __builtin_memset (x, 23, 256);
  v16qi resqi = *(v16qi *)&x[16];
  v8hi reshi = *(v8hi *)&x[16];
  v4si ressi = *(v4si *)&x[16];
  *(v16qi *)dest = resqi;
  *(v8hi *)(dest + 16) = reshi;
  *(v4si *)(dest + 32) = ressi;
}

/* { dg-final { scan-tree-dump-times "Replaced MEM" 3 "fre1" } } */
/* { dg-final { scan-tree-dump-times "Deleted dead call" 1 "dse1" } } */

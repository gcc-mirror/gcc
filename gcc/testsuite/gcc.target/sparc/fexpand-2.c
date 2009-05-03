/* { dg-do compile } */
/* { dg-options "-O1 -mcpu=ultrasparc -mvis -fdump-tree-optimized" } */
typedef short vec16 __attribute__((vector_size(8)));
typedef unsigned char vec8 __attribute__((vector_size(4)));

vec16 foo () {
  vec8 a = {(unsigned char)1,(unsigned char)2,(unsigned char)4,(unsigned char)8};
  return __builtin_vis_fexpand (a);
}

/* { dg-final { scan-tree-dump "{ 16, 32, 64, 128 }" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

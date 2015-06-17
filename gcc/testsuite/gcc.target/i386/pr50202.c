/* { dg-do compile } */
/* { dg-options "-O -fno-tree-dse -fno-dce -msse4" } */

typedef char __v16qi __attribute__ ((__vector_size__ (16)));

__v16qi v;
int i;

void
foo (void)
{
  i = __builtin_ia32_pcmpistri128 (v, v, 255);
  i = 255;
}

/* { dg-do compile } */
/* { dg-options "-O2" } */
unsigned int foo(unsigned int x)
{
  return __builtin_nvptx_brev(x);
}

/* { dg-final { scan-assembler "brev.b32" } } */

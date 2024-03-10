/* { dg-do compile } */
/* { dg-options "-O2" } */
unsigned long foo(unsigned long x)
{
  return __builtin_nvptx_brevll(x);
}

/* { dg-final { scan-assembler "brev.b64" } } */

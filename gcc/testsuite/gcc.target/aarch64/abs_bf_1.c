/* { dg-options "-O2 -ffast-math" } */

void
foo (__bf16 *ptr)
{
  for (int i = 0; i < 8; ++i)
    ptr[i] = __builtin_fabsf (ptr[i]);
}

/* { dg-final { scan-assembler {\t(?:bic|and)\t[zv]} } } */

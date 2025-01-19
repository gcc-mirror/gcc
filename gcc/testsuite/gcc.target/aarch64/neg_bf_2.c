/* { dg-options "-O2" } */

void
foo (__bf16 *ptr)
{
  for (int i = 0; i < 8; ++i)
    ptr[i] = -ptr[i];
}

/* { dg-final { scan-assembler {\teor\t[zv]} } } */

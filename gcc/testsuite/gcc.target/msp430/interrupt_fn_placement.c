/* { dg-do compile } */
/* { dg-options "-mlarge -mcode-region=either -ffunction-sections" } */
/* { dg-final { scan-assembler-not "\\.either\\.lowtext" } } */

void __attribute__ ((interrupt (2))) ir_1 (void)
{
  while (1);
}

int main (void)
{
  while (1);
}

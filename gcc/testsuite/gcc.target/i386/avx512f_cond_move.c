/* { dg-do compile } */
/* { dg-options "-O3 -mavx512f" } */
/* { dg-final { scan-assembler "(vpblendmd|vmovdqa32)" } } */

unsigned int x[128];
unsigned int y[128];

void
foo () 
{
  int i;
  for (i = 0; i < 128; i++)
    x[i] = y[i] > 3 ? 2 : 0;
}

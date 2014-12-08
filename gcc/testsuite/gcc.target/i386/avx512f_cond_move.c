/* { dg-do compile } */
/* { dg-options "-O3 -mavx512f" } */
/* { dg-final { scan-assembler-times "(?:vpblendmd|vmovdqa32)\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 8 } } */

unsigned int x[128];
unsigned int y[128];

void
foo () 
{
  int i;
  for (i = 0; i < 128; i++)
    x[i] = y[i] > 3 ? 2 : 0;
}

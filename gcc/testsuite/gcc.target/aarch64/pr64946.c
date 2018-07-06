
/* { dg-do compile } */
/* { dg-options "-O3" } */

signed char a[100],b[100];
void absolute_s8 (void)
{
  int i;
  for (i=0; i<16; i++)
    a[i] = (b[i] > 0 ? b[i] : -b[i]);
};

/* { dg-final { scan-assembler-times "abs\tv\[0-9\]+.16b, v\[0-9\]+.16b" 1 } } */

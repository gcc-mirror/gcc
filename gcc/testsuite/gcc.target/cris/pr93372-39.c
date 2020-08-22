/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not {\tcmp|\ttest|\tmovu|\tmovs} } } */
/* { dg-final { scan-assembler-times "\tbound.b" 1 } } */
/* { dg-final { scan-assembler-times "\tbound.w" 1 } } */

unsigned int ub (unsigned int a, unsigned char *b, int *c)
{
  unsigned int d = a < *b ? a : *b;
  *c = d == 0;
  return d;
}

unsigned int us (unsigned int a, unsigned short *b, int *c)
{
  unsigned int d = a < *b ? a : *b;
  *c = d == 0;
  return d;
}

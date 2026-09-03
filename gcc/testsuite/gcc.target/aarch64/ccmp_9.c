/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-reassoc" } */
/* PR middle-end/109832 */

/* One explicit comparison is enough for a chain to be worth a conditional
   compare, so the boolean operands become comparisons against zero.  */

int
bool_and_cmp_last (_Bool a, _Bool b, _Bool c, int x, int y)
{
  return ((a & b) & c) & (x < y);
}

int
bool_and_cmp_first (_Bool a, _Bool b, _Bool c, int x, int y)
{
  return (((x < y) & a) & b) & c;
}

/* { dg-final { scan-assembler-times {\tccmp\t} 6 } } */

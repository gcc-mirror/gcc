/* { dg-do compile } */
/* { dg-options "-O2 -mlsx" } */
/* { dg-final { scan-assembler-times "vsigncov.w" 1 } } */
/* { dg-final { scan-assembler-times "vsigncov.d" 1 } } */

int a[4], b[4];

extern int abs (int);

void
foo1 (void)
{
  for (int i = 0; i < 4; i++)
    a[i] = abs (b[i]);
}

long la[2], lb[2];

extern long labs (long);

void
foo2 (void)
{
  for (int i = 0; i < 2; i++)
    la[i] = labs (lb[i]);
}

/* { dg-do compile } */
/* { dg-options "-O2 -mlasx" } */
/* { dg-final { scan-assembler "\tvrotr\.w\t" } } */
/* { dg-final { scan-assembler "\txvrotr\.w\t" } } */
/* { dg-final { scan-assembler "\tvrotri\.w\t\[^\n\]*7\n" } } */
/* { dg-final { scan-assembler "\txvrotri\.w\t\[^\n\]*7\n" } } */

unsigned int a[8], b[8];

void
test1 (void)
{
  for (int i = 0; i < 4; i++)
    a[i] = a[i] >> b[i] | a[i] << (32 - b[i]);
}

void
test2 (void)
{
  for (int i = 0; i < 8; i++)
    a[i] = a[i] >> b[i] | a[i] << (32 - b[i]);
}

void
test3 (void)
{
  for (int i = 0; i < 4; i++)
    a[i] = a[i] >> 7 | a[i] << 25;
}

void
test4 (void)
{
  for (int i = 0; i < 8; i++)
    a[i] = a[i] >> 7 | a[i] << 25;
}

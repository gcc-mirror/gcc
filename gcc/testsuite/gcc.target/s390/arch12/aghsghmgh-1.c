/* { dg-compile } */

long long
agh (long long a, short int *p)
{
  return a + *p;
}

long long
sgh (long long a, short int *p)
{
  return a - *p;
}

long long
mgh (long long a, short int *p)
{
  return a * *p;
}

/* { dg-final { scan-assembler-times "\tagh\t" 1 } } */
/* { dg-final { scan-assembler-times "\tsgh\t" 1 } } */
/* { dg-final { scan-assembler-times "\tmgh\t" 1 } } */

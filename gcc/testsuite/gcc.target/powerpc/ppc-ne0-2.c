/* PR target/51274 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-times {\maddic\M} 3 } } */
/* { dg-final { scan-assembler-times {\maddze\M} 3 } } */

long plus_ne0(long a, long b)
{
  return (a != 0) + b;
}

void dummy(void);

void cmp_plus_ne0(long a, long b)
{
  if ((a != 0) + b)
    dummy();
}

long plus_ne0_cmp(long a, long b)
{
  a = (a != 0) + b;
  if (a)
    dummy();
  return a;
}

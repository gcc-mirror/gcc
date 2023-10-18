/* { dg-do compile } */
/* { dg-options "-O2" } */
/* PR target/110986 */


long long full(unsigned a, unsigned b)
{
  return a ? ~b : b;
}
unsigned fuu(unsigned a, unsigned b)
{
  return a ? ~b : b;
}
long long fllll(unsigned long long a, unsigned long long b)
{
  return a ? ~b : b;
}

/* { dg-final { scan-assembler-times "csinv\tw\[0-9\]*" 2 } } */
/* { dg-final { scan-assembler-times "csinv\tx\[0-9\]*" 1 } } */

/* { dg-do compile } */
/* { dg-options "-O2 -mno-stv" } */
/* { dg-final { scan-assembler "test" } } */
/* { dg-final { scan-assembler-not "cmp" } } */
#define max(a,b) (((a) > (b))? (a) : (b))
unsigned int
t(unsigned int a)
{
  return (max(a,1));
}

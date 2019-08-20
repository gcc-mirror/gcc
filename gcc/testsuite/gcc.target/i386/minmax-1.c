/* { dg-do compile } */
/* { dg-options "-O2 -march=opteron -mno-stv" } */
/* { dg-final { scan-assembler "test" } } */
/* { dg-final { scan-assembler-not "cmp" } } */
#define max(a,b) (((a) > (b))? (a) : (b))
int
t(int a)
{
  return (max(a,1));
}

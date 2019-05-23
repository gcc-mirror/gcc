/* { dg-do compile } */
/* { dg-require-effective-target mfentry } */
/* { dg-options "-pg -mfentry -mfentry-name=foo" } */
/* { dg-final { scan-assembler "call.*foo" } } */
/* { dg-final { scan-assembler "call.*bar" } } */

int func(int a)
{
  return a+1;
}

__attribute__((fentry_name("bar")))
int func2(int a)
{
  return a+1;
}

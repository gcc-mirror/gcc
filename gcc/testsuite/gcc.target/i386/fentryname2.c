/* { dg-do compile } */
/* { dg-require-effective-target mfentry } */
/* { dg-require-profiling "-pg" } */
/* { dg-options "-pg -mfentry -mrecord-mcount -mfentry-section=foo" } */
/* { dg-final { scan-assembler "section.*foo" } } */
/* { dg-final { scan-assembler "section.*bar" } } */

int func(int a)
{
  return a+1;
}

__attribute__((fentry_section("bar")))
int func2(int a)
{
  return a+1;
}

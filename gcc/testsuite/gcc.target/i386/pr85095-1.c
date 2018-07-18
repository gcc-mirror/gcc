/* PR target/85095 *
/* { dg-do compile } */
/* { dg-options "-O2 -masm=att" } */

unsigned int
foo (unsigned int a, unsigned int b)
{
  a += b;
  if (a < b) a++;
  return a;
}

#ifdef __x86_64__
unsigned long long
bar (unsigned long long a, unsigned long long b)
{
  a += b;
  if (a < b) a++;
  return a;
}

unsigned long long
baz (unsigned int a, unsigned int b)
{
  a += b;
  if (a < b) a++;
  return a;
}
#endif

/* { dg-final { scan-assembler-times "adcl\t\\\$0," 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "adcl\t\\\$0," 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "adcq\t\\\$0," 1 { target { ! ia32 } } } } */

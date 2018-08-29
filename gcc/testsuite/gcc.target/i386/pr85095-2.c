/* PR target/85095 *
/* { dg-do compile } */
/* { dg-options "-O2 -masm=att" } */

unsigned int
f1 (unsigned int a, unsigned int b)
{
  unsigned int i = __builtin_add_overflow (a, b, &a);
  return a + i;
}

unsigned int
f2 (unsigned int a, unsigned int b)
{
  unsigned int i = __builtin_add_overflow (a, b, &a);
  return a - i;
}

#ifdef __x86_64__
unsigned long long
f3 (unsigned long long a, unsigned long long b)
{
  unsigned long long i = __builtin_add_overflow (a, b, &a);
  return a + i;
}

unsigned long long
f4 (unsigned long long a, unsigned long long b)
{
  unsigned long long i = __builtin_add_overflow (a, b, &a);
  return a - i;
}

unsigned long long
f5 (unsigned int a, unsigned int b)
{
  unsigned int i = __builtin_add_overflow (a, b, &a);
  return a + i;
}

unsigned long long
f6 (unsigned int a, unsigned int b)
{
  unsigned int i = __builtin_add_overflow (a, b, &a);
  return a - i;
}
#endif

/* { dg-final { scan-assembler-times "adcl\t\\\$0," 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "sbbl\t\\\$0," 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "adcl\t\\\$0," 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "sbbl\t\\\$0," 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "adcq\t\\\$0," 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "sbbq\t\\\$0," 1 { target { ! ia32 } } } } */

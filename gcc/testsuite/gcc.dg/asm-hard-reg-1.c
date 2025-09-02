/* { dg-do compile { target aarch64*-*-* arm*-*-* i?86-*-* powerpc*-*-* riscv*-*-* s390*-*-* x86_64-*-* } } */

#if defined (__aarch64__)
# define GPR "{x4}"
/* { dg-final { scan-assembler-times "foo\tx4" 8 { target { aarch64*-*-* } } } } */
#elif defined (__arm__)
# define GPR "{r4}"
/* { dg-final { scan-assembler-times "foo\tr4" 8 { target { arm*-*-* } } } } */
#elif defined (__i386__)
# define GPR "{ecx}"
/* { dg-final { scan-assembler-times "foo\t%cl" 2 { target { i?86-*-* } } } } */
/* { dg-final { scan-assembler-times "foo\t%cx" 2 { target { i?86-*-* } } } } */
/* { dg-final { scan-assembler-times "foo\t%ecx" 4 { target { i?86-*-* } } } } */
#elif defined (__powerpc__) || defined (__POWERPC__)
# define GPR "{r5}"
/* { dg-final { scan-assembler-times "foo\t5" 8 { target { powerpc*-*-* } } } } */
#elif defined (__riscv)
# define GPR "{t5}"
/* { dg-final { scan-assembler-times "foo\tt5" 8 { target { riscv*-*-* } } } } */
#elif defined (__s390__)
# define GPR "{r4}"
/* { dg-final { scan-assembler-times "foo\t%r4" 8 { target { s390*-*-* } } } } */
#elif defined (__x86_64__)
# define GPR "{rcx}"
/* { dg-final { scan-assembler-times "foo\t%cl" 2 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "foo\t%cx" 2 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "foo\t%ecx" 2 { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-assembler-times "foo\t%rcx" 2 { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-assembler-times "foo\t%ecx" 4 { target { { i?86-*-* x86_64-*-* } && { ! lp64 } } } } } */
#endif

char
test_char (char x)
{
  __asm__ ("foo\t%0" : "+"GPR (x));
  return x;
}

char
test_char_from_mem (char *x)
{
  __asm__ ("foo\t%0" : "+"GPR (*x));
  return *x;
}

short
test_short (short x)
{
  __asm__ ("foo\t%0" : "+"GPR (x));
  return x;
}

short
test_short_from_mem (short *x)
{
  __asm__ ("foo\t%0" : "+"GPR (*x));
  return *x;
}

int
test_int (int x)
{
  __asm__ ("foo\t%0" : "+"GPR (x));
  return x;
}

int
test_int_from_mem (int *x)
{
  __asm__ ("foo\t%0" : "+"GPR (*x));
  return *x;
}

long
test_long (long x)
{
  __asm__ ("foo\t%0" : "+"GPR (x));
  return x;
}

long
test_long_from_mem (long *x)
{
  __asm__ ("foo\t%0" : "+"GPR (*x));
  return *x;
}

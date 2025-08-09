/* { dg-do compile { target { { aarch64*-*-* powerpc64*-*-* riscv64-*-* s390*-*-* } || { { i?86-*-* x86_64-*-* } && { ! ia32 } } } } } */
/* { dg-options "-std=c99" } we need long long */

#if defined (__aarch64__)
# define GPR "{x4}"
/* { dg-final { scan-assembler-times "foo\tx4" 2 { target { aarch64*-*-* } } } } */
#elif defined (__powerpc__) || defined (__POWERPC__)
# define GPR "{r5}"
/* { dg-final { scan-assembler-times "foo\t5" 2 { target { powerpc64*-*-* } } } } */
#elif defined (__riscv)
# define GPR "{t5}"
/* { dg-final { scan-assembler-times "foo\tt5" 2 { target { riscv64-*-* } } } } */
#elif defined (__s390__)
# define GPR "{r4}"
/* { dg-final { scan-assembler-times "foo\t%r4" 2 { target { s390*-*-* } } } } */
#elif defined (__x86_64__)
# define GPR "{rcx}"
/* { dg-final { scan-assembler-times "foo\t%rcx" 2 { target { i?86-*-* x86_64-*-* } } } } */
#endif

long long
test_longlong (long long x)
{
  __asm__ ("foo\t%0" : "+"GPR (x));
  return x;
}

long long
test_longlong_from_mem (long long *x)
{
  __asm__ ("foo\t%0" : "+"GPR (*x));
  return *x;
}

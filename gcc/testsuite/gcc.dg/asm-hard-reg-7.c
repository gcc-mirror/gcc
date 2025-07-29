/* { dg-do compile { target aarch64*-*-* arm*-*-* i?86-*-* powerpc*-*-* riscv*-*-* s390*-*-* x86_64-*-* } } */
/* { dg-options "-O2" } */

/* Test multiple alternatives.  */

#if defined (__aarch64__)
# define GPR "{x1}"
/* { dg-final { scan-assembler-times "foo\tx1,x1" 2 { target { aarch64*-*-* } } } } */
#elif defined (__arm__)
# define GPR "{r1}"
/* { dg-final { scan-assembler-times "foo\tr1,r1" 2 { target { arm*-*-* } } } } */
#elif defined (__i386__)
# define GPR "{eax}"
/* { dg-final { scan-assembler-times "foo\t%eax,%eax" 2 { target { i?86-*-* } } } } */
#elif defined (__powerpc__) || defined (__POWERPC__)
# define GPR "{r4}"
/* { dg-final { scan-assembler-times "foo\t4,4" 2 { target { powerpc*-*-* } } } } */
#elif defined (__riscv)
# define GPR "{t1}"
/* { dg-final { scan-assembler-times "foo\tt1,t1" 2 { target { riscv*-*-* } } } } */
#elif defined (__s390__)
# define GPR "{r0}"
/* { dg-final { scan-assembler-times "foo\t%r0,%r0" 2 { target { s390*-*-* } } } } */
#elif defined (__x86_64__)
# define GPR "{eax}"
/* { dg-final { scan-assembler-times "foo\t%eax,%eax" 2 { target { x86_64-*-* } } } } */
#endif

int
test_1 (int x)
{
  __asm__ ("foo\t%0,%0" : "+"GPR (x));
  return x;
}

int
test_2 (int x, int y)
{
  __asm__ ("foo\t%0,%1" : "="GPR (x) : GPR (y));
  return x;
}

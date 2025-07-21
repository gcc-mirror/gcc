/* { dg-do compile { target { { aarch64*-*-* powerpc64*-*-* riscv64-*-* s390*-*-* x86_64-*-* } && int128 } } } */
/* { dg-options "-O2" } get rid of -ansi since we use __int128 */

#if defined (__aarch64__)
# define REG "{x4}"
/* { dg-final { scan-assembler-times "foo\tx4" 1 { target { aarch64*-*-* } } } } */
#elif defined (__powerpc__) || defined (__POWERPC__)
# define REG "{r5}"
/* { dg-final { scan-assembler-times "foo\t5" 1 { target { powerpc*-*-* } } } } */
#elif defined (__riscv)
# define REG "{t5}"
/* { dg-final { scan-assembler-times "foo\tt5" 1 { target { riscv*-*-* } } } } */
#elif defined (__s390__)
# define REG "{r4}"
/* { dg-final { scan-assembler-times "foo\t%r4" 1 { target { s390*-*-* } } } } */
#elif defined (__x86_64__)
# define REG "{xmm0}"
/* { dg-final { scan-assembler-times "foo\t%xmm0" 1 { target { x86_64-*-* } } } } */
#endif

void
test (void)
{
  __asm__ ("foo\t%0" :: REG ((__int128) 42));
}

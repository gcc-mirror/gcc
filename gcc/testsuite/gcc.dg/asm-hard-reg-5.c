/* { dg-do compile { target { { aarch64*-*-* powerpc64*-*-* riscv64-*-* s390*-*-* } || { { i?86-*-* x86_64-*-* } && { ! ia32 } } } } } */

typedef int V __attribute__ ((vector_size (4 * sizeof (int))));

#if defined (__aarch64__)
# define VR "{v20}"
/* { dg-final { scan-assembler-times "foo\tv20" 2 { target { aarch64*-*-* } } } } */
#elif defined (__powerpc__) || defined (__POWERPC__)
# define VR "{v5}"
/* { dg-final { scan-assembler-times "foo\t5" 2 { target { powerpc64*-*-* } } } } */
#elif defined (__riscv)
# define VR "{v5}"
/* { dg-additional-options "-march=rv64imv" { target riscv64-*-* } } */
/* { dg-final { scan-assembler-times "foo\tv5" 2 { target { riscv*-*-* } } } } */
#elif defined (__s390__)
# define VR "{v5}"
/* { dg-require-effective-target s390_mvx { target s390*-*-* } } */
/* { dg-final { scan-assembler-times "foo\t%v5" 2 { target s390*-*-* } } } */
#elif defined (__x86_64__)
# define VR "{xmm9}"
/* { dg-final { scan-assembler-times "foo\t%xmm9" 2 { target { x86_64-*-* } } } } */
#endif

V
test (V x)
{
  __asm__ ("foo\t%0" : "+"VR (x));
  return x;
}

V
test_from_mem (V *x)
{
  __asm__ ("foo\t%0" : "+"VR (*x));
  return *x;
}

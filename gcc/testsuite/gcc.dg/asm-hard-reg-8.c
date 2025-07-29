/* { dg-do compile { target aarch64*-*-* arm*-*-* i?86-*-* powerpc*-*-* riscv*-*-* s390*-*-* x86_64-*-* } } */

/* Due to hard register constraints, X must be copied.  */

#if defined (__aarch64__)
# define GPR1 "{x1}"
# define GPR2 "{x2}"
#elif defined (__arm__)
# define GPR1 "{r1}"
# define GPR2 "{r2}"
#elif defined (__i386__)
# define GPR1 "{eax}"
# define GPR2 "{ebx}"
#elif defined (__powerpc__) || defined (__POWERPC__)
# define GPR1 "{r4}"
# define GPR2 "{r5}"
#elif defined (__riscv)
# define GPR1 "{t1}"
# define GPR2 "{t2}"
#elif defined (__s390__)
# define GPR1 "{r0}"
# define GPR2 "{r1}"
#elif defined (__x86_64__)
# define GPR1 "{eax}"
# define GPR2 "{ebx}"
#endif

#define TEST(T) \
int \
test_##T (T x) \
{ \
  int out; \
  __asm__ ("foo" : "=r" (out) : GPR1 (x), GPR2 (x)); \
  return out; \
}

TEST(char)
TEST(short)
TEST(int)
TEST(long)

int
test_subreg (long x)
{
  int out;
  short subreg_x = x;
  __asm__ ("foo" : "=r" (out) : GPR1 (x), GPR2 (subreg_x));
  return out;
}

/* PR rtl-optimization/89313  */
/* { dg-do compile { target aarch64*-*-* arm*-*-* i?86-*-* powerpc*-*-* s390*-*-* x86_64-*-* } } */
/* { dg-options "-O2" } */

#if defined (__aarch64__)
# define REG "x0"
#elif defined (__arm__)
# define REG "r0"
#elif defined (__i386__)
# define REG "%eax"
#elif defined (__powerpc__) || defined (__POWERPC__)
# define REG "r3"
#elif defined (__s390__)
# define REG "0"
#elif defined (__x86_64__)
# define REG "rax"
#endif

long
bug (long arg)
{
  register long output asm (REG);
  long input = arg;
  asm ("blah %0, %1, %2" : "=&r" (output) : "r" (input), "0" (input));
  return output;
}

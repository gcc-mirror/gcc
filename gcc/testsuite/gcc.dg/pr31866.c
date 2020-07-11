/* PR tree-optimization/31866 */
/* { dg-do compile { target alpha*-*-* cris-*-* i?86-*-* mmix-*-* powerpc*-*-* rs6000-*-* x86_64-*-* } } */
/* { dg-options "-O2" } */

#if defined (__alpha__)
# define REG "$1"
#elif defined (__CRIS__)
# define REG "r10"
#elif defined (__i386__)
# define REG "%eax"
#elif defined (__MMIX__)
# define REG "$8"
#elif defined (__powerpc__) || defined (__PPC__) || defined (__ppc__) \
	|| defined (__POWERPC__) || defined (PPC) || defined (_IBMR2)
# define REG "6"
#elif defined (__x86_64__)
# define REG "rax"
#endif

long int
foo (void)
{
  unsigned long int s;
  long int a = (long int) 0;
  register long int r asm (REG) = a;
  asm ("" : "=r" (s) : "0" (r));
  return s;
}

/* PR 14204 */
/* { dg-do compile } */
/* { dg-options "-O -Wall -Werror" } */

#if defined __alpha__
# define ASM __asm__("$30")
#elif defined __i386__
# define ASM __asm__("esp")
#elif defined (__powerpc__) || defined (__PPC__) || defined (__ppc__) || defined (_POWER)
# define ASM __asm__("r1")
#elif defined __s390__
# define ASM __asm__("r15")
#else
# define ASM
#endif

void *load_PCB (void)
{
  register void *sp ASM;
  return sp;			/* { dg-bogus "uninitialized" } */
}

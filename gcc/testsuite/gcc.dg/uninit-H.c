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
#elif defined __mips
# define ASM __asm__("$sp")
#elif defined __sparc__
# define ASM __asm__("sp")
#elif defined __ia64__
# define ASM __asm__("r12")
#elif defined __hppa__
# define ASM __asm__("%r30")
#elif defined __xtensa__
# define ASM __asm__("sp")
#else
/* The register name should be target-dependent so for other targets,
   we just silence the test.  */
# define ASM = 0
#endif

void *load_PCB (void)
{
  register void *sp ASM;
  return sp;			/* { dg-bogus "uninitialized" } */
}

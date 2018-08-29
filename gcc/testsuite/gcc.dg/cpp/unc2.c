/* Tests for unterminated conditionals: 2.  */
/* { dg-do preprocess } */

#ifdef __sparc__  /* { dg-error "-:unterminated" "unterminated if-elif-elif..." } */
sparc
#elif defined __powerpc__
ppc
#elif defined __i386__
i386
#elif defined __vax__
vax


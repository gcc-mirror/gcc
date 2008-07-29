/* PR middle-end/32370 */
/* { dg-do compile { target i?86-*-* x86_64-*-* ia64-*-* } } */
/* { dg-options "" { target ia64-*-* } } */
/* { dg-options "-ffixed-esi" { target i?86-*-* x86_64-*-* } } */

#if defined __i386__ || defined __x86_64__
# define C "=S"
# define TYPE unsigned long
#elif defined __ia64__
# define C "=a"
# define TYPE unsigned long long
#endif

unsigned int
foo (TYPE port)
{
  unsigned int v;
  __asm__ __volatile__ ("" : C (v) : "Nd" (port));	/* { dg-error "while reloading\|has impossible" } */
  return v;
}

void
bar (void)
{
  foo (0);
}

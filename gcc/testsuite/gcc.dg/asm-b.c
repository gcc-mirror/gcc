/* { dg-do run  { target powerpc-*-* i?386-*-* x86_64-*-* } } */
/* { dg-options "-O1" } */
/* Test to make sure that inline-asm causes the tree optimizers to get the
   V_MAY_DEFs and clobber memory.  */
/* Test from Jakub Jelinek, modified by Andrew Pinski to work on all powerpc targets.  */
extern void abort (void);

unsigned short v = 0x0300;

void
foo (unsigned short *p)
{
  *p = v;
}

int
bar (void)
{
  unsigned short x;
  volatile unsigned short *z;
  foo (&x);
  const unsigned int y = x;
  z = &x;
#if defined (__powerpc__) || defined (__PPC__) || defined (__ppc__) || defined (_POWER)
  __asm __volatile ("sthbrx %1,0,%2" : "=m" (*z) : "r" (y), "r" (z));
#elif defined __i386__ || defined __x86_64__
  __asm __volatile ("movb %b1,1(%2); movb %h1,(%2)" : "=m" (*z) : "r" (y), "r"
(z));
#endif
  return (x & 1) == 0;
}

int
main (void)
{
  if (bar ())
    abort ();
  return 0;
}

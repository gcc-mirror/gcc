/* PR inline-asm/35160 */
/* { dg-do run } */
/* { dg-skip-if "" { ia32 && { ! nonpic } } } */
/* { dg-options "-O2" } */

extern void abort (void);

void
__attribute__((noinline))
foo (unsigned int *y)
{
   unsigned int c0, c1, c2, d0, d1, d2;
   d0 = 0; d1 = 0; d2 = 0; c0 = c1 = c2 = 0;

   __asm__ ("movl $7, %k0; movl $8, %k1; movl $9, %k2"
	    : "+r" (d0), "+r" (d1), "+r" (d2));
   __asm__ ("movl %3, %0; movl %4, %1; movl %5, %2"
	    : "+r" (c0), "+r" (c1), "+r" (c2), "+r" (d0), "+r" (d1), "+r" (d2));
   y[0] = c0;
   y[1] = c1;
   y[2] = c2;
}

int
main (void)
{
  unsigned int y[3];
  foo (y);
  if (y[0] != 7 || y[1] != 8 || y[2] != 9)
    abort ();
  return 0;
}

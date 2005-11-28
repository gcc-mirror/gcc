/* PR inline-asm/6806 */
/* { dg-do run { target i?86-*-* } } */
/* { dg-skip-if "" { i?86-*-* } { "-fpic" "-fPIC" } { "" } } */
/* { dg-options "-O2" } */

extern void abort (void);

volatile int out = 1;
volatile int a = 2;
volatile int b = 4;
volatile int c = 8;
volatile int d = 16;
volatile int e = 32;
volatile int f = 64;

int
main ()
{
  asm volatile ("xorl %%eax, %%eax	\n\t"
		"xorl %%esi, %%esi	\n\t"
		"addl %1, %0		\n\t"
		"addl %2, %0		\n\t"
		"addl %3, %0		\n\t"
		"addl %4, %0		\n\t"
		"addl %5, %0		\n\t"
		"addl %6, %0"
		: "+r" (out)
		: "r" (a), "r" (b), "r" (c), "g" (d), "g" (e), "g" (f)
		: "%eax", "%esi");

  if (out != 127)
    abort ();

  return 0;
}

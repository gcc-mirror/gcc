/* PR inline-asm/30505 */
/* { dg-do compile { target ilp32 } } */
/* { dg-options "-O2" } */

unsigned long long a, c;
unsigned int b, d;

void
test ()
{
  unsigned int e, f;

  __asm__ ("divl %5;movl %1, %0;movl %4, %1;divl %5"
	   : "=&rm" (e), "=a" (f), "=d" (d)
	   : "1" ((unsigned int) (a >> 32)), "g" ((unsigned int) a),
	     "rm" (b), "2" (0)
	   : "cc");
  c = (unsigned long long) e << 32 | f;
}

/* Test floating point division on ia64.  There was a bug in the
   max-throughput version of the inline division code.  Expecting an
   exact value from a floating point expression is unwise but GCC
   depends on it in allocno_compare.  */

/* { dg-do run } */
/* { dg-options "-minline-float-divide-max-throughput" } */

extern void abort (void);

volatile int i = 24;
volatile int j = 30;
volatile int k = 1;

int main()
{
        int pri2 = (((double) i / j) * (10000 / 1000) * k);
        if (pri2 != 8) abort();
	return 0;
}

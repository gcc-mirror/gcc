/* Test integer mod on ia64.  There was a bug in the inline integer
   division code. */

/* { dg-do run } */
/* { dg-options "-minline-int-divide-max-throughput" { target ia64-*-* } } */

extern void abort (void);

volatile int i = 10;
volatile int j = 10;

int main()
{
        int k = i % j;
        if (k != 0) abort();
	return 0;
}

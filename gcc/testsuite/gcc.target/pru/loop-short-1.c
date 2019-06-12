/* Test LOOP generation for very short loops. */

/* { dg-do run } */
/* { dg-options "-O1 -mloop" } */

/* -O1 in the options is significant.  Without it do-loop will not be
   run.  */

extern void abort (void);

volatile unsigned int int_12345 = 12345;
volatile unsigned int int_0 = 0;
volatile unsigned int int_1 = 1;

unsigned int
test_loop_sum (unsigned int n)
{
	unsigned i;
	volatile unsigned int s = 0;
	for (i = 0; i < n; i++) {
		s++;
	}
	return s;
}

unsigned int
test_loop_shift20 (unsigned int n)
{
	unsigned i;
	for (i = 0; i < 10; i++) {
		n <<= 2;
	}
	return n;
}

int
main (int argc, char** argv)
{
  if (test_loop_sum (int_0) != 0)
    abort();
  if (test_loop_sum (int_1) != 1)
    abort();
  if (test_loop_sum (int_12345) != 12345)
    abort();

  if (test_loop_shift20 (int_0) != 0)
    abort();
  if (test_loop_shift20 (int_1) != (1u << 20))
    abort();

  return 0;
}


/* Test LOOP generation for do while.
   Ensure the post-condition "do while" is correctly translated
   to a pre-condition PRU LOOP instruction.  */

/* { dg-do run } */
/* { dg-options "-O1 -mloop" } */

/* -O1 in the options is significant.  Without it do-loop will not be
   run.  */

extern void abort (void);

volatile unsigned int int_12345 = 12345;
volatile unsigned int int_0 = 0;
volatile unsigned int int_1 = 1;

unsigned int
test_loop (unsigned int n)
{
  unsigned int i = 0;
  volatile unsigned int s = 0;

  if (n >= 0x7fff) return 0;

  do {
    s++;
    i++;
  } while (i < n);
  return s;
}


int
main (int argc, char** argv)
{
  if (test_loop (int_0) != 1)
    abort();
  if (test_loop (int_1) != 1)
    abort();
  if (test_loop (int_12345) != 12345)
    abort();

  return 0;
}


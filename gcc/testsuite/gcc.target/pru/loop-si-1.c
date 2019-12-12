/* Test LOOP recognition */

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
  unsigned int i;
  volatile unsigned int s = 0;

  if (n >= 0x10000) return 0;

  for (i = 0; i < n; i++)
    s++;
  return s;
}


int
main (int argc, char** argv)
{
  if (test_loop (int_0) != 0)
    abort();
  if (test_loop (int_1) != 1)
    abort();
  if (test_loop (int_12345) != 12345)
    abort();

  return 0;
}


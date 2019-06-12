/* Test LOOP recognition - short ints*/

/* { dg-do run } */
/* { dg-options "-O1 -mloop" } */

/* -O1 in the options is significant.  Without it do-loop will not be
   run.  */

extern void abort (void);

volatile unsigned short short_12345 = 12345;
volatile unsigned short short_0 = 0;
volatile unsigned short short_1 = 1;

unsigned int
test_loop_short (unsigned short n)
{
  unsigned short i;
  volatile unsigned int s = 0;

  for (i = 0; i < n; i++)
    s++;
  return s;
}

int
main (int argc, char** argv)
{
  if (test_loop_short (short_0) != 0)
    abort();
  if (test_loop_short (short_1) != 1)
    abort();
  if (test_loop_short (short_12345) != 12345)
    abort();

  return 0;
}


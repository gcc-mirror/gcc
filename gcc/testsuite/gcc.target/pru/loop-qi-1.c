/* Test LOOP recognition */

/* { dg-do run } */
/* { dg-options "-O1 -mloop" } */

/* -O1 in the options is significant.  Without it do-loop will not be
   run.  */

extern void abort (void);

volatile unsigned char char_123 = 123;
volatile unsigned char char_0 = 0;
volatile unsigned char char_1 = 1;

unsigned int
test_loop_char (unsigned char n)
{
  unsigned char i;
  volatile unsigned int s = 0;

  for (i = 0; i < n; i++)
    s++;
  return s;
}

int
main (int argc, char** argv)
{
  if (test_loop_char (char_0) != 0)
    abort();
  if (test_loop_char (char_1) != 1)
    abort();
  if (test_loop_char (char_123) != 123)
    abort();

  return 0;
}


/* Test LOOP recognition */

/* { dg-do run } */
/* { dg-options "-O1 -mloop" } */

/* -O1 in the options is significant.  Without it do-loop will not be
   run.  */

extern void abort (void);

unsigned int
test_loop_ubyte_101 (void)
{
  unsigned int i;
  volatile unsigned int s = 0;

  for (i = 0; i < 101; i++)
    s++;
  return s;
}

int
main (int argc, char** argv)
{
  if (test_loop_ubyte_101 () != 101)
    abort();

  return 0;
}


/* Shared test code for the various __RTL tests of times_two that
   start at different passes.  */

extern void abort (void);
int times_two (int i);

int main (void)
{
  if (times_two (0) != 0)
    abort ();

  if (times_two (1) != 2)
    abort ();

  if (times_two (100) != 200)
    abort ();

  if (times_two (-20) != -40)
    abort ();

  return 0;
}

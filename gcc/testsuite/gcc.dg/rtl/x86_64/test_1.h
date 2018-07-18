/* Shared test code for the various __RTL tests of test_1 that
   start at different passes.  */

extern void abort (void);
extern int test_1 (int i, int j, int k);

int main (void)
{
  if (test_1 (0, 0, 3) != -3)
    abort ();

  if (test_1 (0, 1, 3) != 7)
    abort ();

  return 0;
}

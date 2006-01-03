extern void abort ();

int f(int x)
{
  return (x >> 31) ? -1 : 1;
}

int main (int argc)
{
  /* Test that the function above returns different values for
     different signs.  */
  if (f(argc) == f(-argc))
    abort ();
  return 0;
}


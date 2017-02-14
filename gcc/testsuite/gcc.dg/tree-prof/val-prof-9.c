/* { dg-options "-O0 -fdump-tree-optimized" } */

int
main (int argc, char **argv)
{
  unsigned u = (argc - 1);
  int counter = 0;

  for (unsigned i = 0; i < 100; i++)
  {
    counter += u % 16;
  }

  return counter;
}

/* autofdo does not do value profiling so far */
/* { dg-final-use-not-autofdo { scan-tree-dump-times "__gcov_pow2_profiler" 0 "optimized" } } */

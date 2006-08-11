extern void abort (void);
int
foo (unsigned int u)
{
  return (int)(u + 4) < (int)u;
}

int
main (int argc, char *argv[])
{
  unsigned int u;

  /* Run with no arguments so u will be MAX_INT and the optimizers
     won't know its value.  */
  if (argc > 1)
    u = 1;
  else
    u = 0x7fffffff;

  if (foo (u) == 0)
    abort();
  return 0;
}


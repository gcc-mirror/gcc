#ifndef HAVE_DEFINED_SCALAR_SAT_BINARY_RUN_XXX
#define HAVE_DEFINED_SCALAR_SAT_BINARY_RUN_XXX

int
main ()
{
  unsigned i;
  T d;

  for (i = 0; i < sizeof (DATA) / sizeof (DATA[0]); i++)
    {
      d = DATA[i];

      if (RUN_BINARY (d.a, d.b) != d.expect)
	__builtin_abort ();
    }

  return 0;
}

#endif

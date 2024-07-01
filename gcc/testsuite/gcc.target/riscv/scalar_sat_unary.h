#ifndef HAVE_DEFINED_SCALAR_SAT_UNARY
#define HAVE_DEFINED_SCALAR_SAT_UNARY

int
main ()
{
  unsigned i;
  T d;

  for (i = 0; i < sizeof (DATA) / sizeof (DATA[0]); i++)
    {
      d = DATA[i];

      if (RUN_UNARY (d.from) != d.to)
	__builtin_abort ();
    }

  return 0;
}

#endif


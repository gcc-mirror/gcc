void
test_isunordered(double x, double y, int true)
{
  if (__builtin_isunordered(x, y))
    {
      if (! true)
	abort ();
    }
  else
    {
      if (true)
	abort ();
    }
}

void
test_isless(double x, double y, int true)
{
  if (__builtin_isless(x, y))
    {
      if (! true)
	abort ();
    }
  else
    {
      if (true)
	abort ();
    }
}

void
test_islessequal(double x, double y, int true)
{
  if (__builtin_islessequal(x, y))
    {
      if (! true)
	abort ();
    }
  else
    {
      if (true)
	abort ();
    }
}

void
test_isgreater(double x, double y, int true)
{
  if (__builtin_isgreater(x, y))
    {
      if (! true)
	abort ();
    }
  else
    {
      if (true)
	abort ();
    }
}

void
test_isgreaterequal(double x, double y, int true)
{
  if (__builtin_isgreaterequal(x, y))
    {
      if (! true)
	abort ();
    }
  else
    {
      if (true)
	abort ();
    }
}

void
test_islessgreater(double x, double y, int true)
{
  if (__builtin_islessgreater(x, y))
    {
      if (! true)
	abort ();
    }
  else
    {
      if (true)
	abort ();
    }
}

#define NAN (0.0 / 0.0)

int
main()
{
  struct try
  {
    double x, y;
    unsigned unord : 1;
    unsigned lt : 1;
    unsigned le : 1;
    unsigned gt : 1;
    unsigned ge : 1;
    unsigned lg : 1;
  };

  static struct try const data[] =
  {
    { NAN, NAN, 1, 0, 0, 0, 0, 0 },
    { 0.0, NAN, 1, 0, 0, 0, 0, 0 },
    { NAN, 0.0, 1, 0, 0, 0, 0, 0 },
    { 0.0, 0.0, 0, 0, 1, 0, 1, 0 },
    { 1.0, 2.0, 0, 1, 1, 0, 0, 1 },
    { 2.0, 1.0, 0, 0, 0, 1, 1, 1 },
  };

  const int n = sizeof(data) / sizeof(data[0]);
  int i;

  for (i = 0; i < n; ++i)
    {
      test_isunordered (data[i].x, data[i].y, data[i].unord);
      test_isless (data[i].x, data[i].y, data[i].lt);
      test_islessequal (data[i].x, data[i].y, data[i].le);
      test_isgreater (data[i].x, data[i].y, data[i].gt);
      test_isgreaterequal (data[i].x, data[i].y, data[i].ge);
      test_islessgreater (data[i].x, data[i].y, data[i].lg);
    }

  exit (0);
}

// { dg-do run }
// { dg-require-effective-target dfp }

extern "C" void abort ();

typedef float dec32 __attribute__((mode(SD)));
typedef float dec64 __attribute__((mode(DD)));
typedef float dec128 __attribute__((mode(TD)));

int
foo (double fp)
{
  if (fp < 32.0)
    throw (dec32)32;
  if (fp < 64.0)
    throw (dec64)64;
  if (fp < 128.0)
    throw (dec128)128;
  return 0;
}

int bar (double fp)
{
  try
    {
      foo (fp);
      abort ();
    }
  catch (dec32 df)
    {
      if (df != (dec32)32)
	abort ();
    }
  catch (dec64 dd)
    {
      if (dd != (dec64)64)
	abort ();
    }
  catch (dec128 dl)
    {
      if (dl != (dec128)128)
	abort ();
    }
  return 0;
}

int
main ()
{
  bar (10.0);
  bar (20.0);
  bar (100.0);
  return 0;
}

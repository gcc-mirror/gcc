// { dg-do run }
// { dg-options "-O2 -frename-registers -fprofile-generate -fomit-frame-pointer" }
// { dg-require-profiling "-fprofile-generate" }
// { dg-require-effective-target exceptions_enabled }
// { dg-final { cleanup-coverage-files } }

/* Verify that regrename cannot use an unsaved frame pointer.  */

extern "C" void abort (void);

struct MyException {};
struct Data {
    int nr;
    Data() : nr(66) {}
};

Data __attribute__((noinline,noclone))
getData (int i)
{
  if (i)
    throw MyException ();
  Data data;
  data.nr = i;
  return data;
}

int
main (int, char **)
{
  Data data;
  try
    {
      data = getData (1);
    }
  catch (MyException &)
    {
      if (data.nr != 66)
	abort ();
    }
  return 0;
}

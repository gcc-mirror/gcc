// Test that cleanups get run when a catch filter fails to match.
// { dg-do run }

extern "C" void exit(int);
extern "C" void abort();

struct a
{
  a();
  ~a();
};

struct e1 {};
struct e2 {};

void
ex_test ()
{
  a aa;
  try
    {
      throw e1 ();
    }
  catch (e2 &)
    {
    }
}

int
main ()
{
  try
    {
      ex_test ();
    }
  catch (...)
    {
    }
  abort ();
}

a::a() { }
a::~a() { exit (0); }

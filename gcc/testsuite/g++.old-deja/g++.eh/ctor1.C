// { dg-do assemble  }
struct A
{
  A();
  A(A&);			// { dg-error "" } referenced below
};

int
main ()
{
  try
    {
      throw A();		// { dg-error "" } can't copy
    }
  catch (...) { }
}

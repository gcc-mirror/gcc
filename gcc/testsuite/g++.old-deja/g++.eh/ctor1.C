struct A
{
  A();
  A(A&);			// ERROR - referenced below
};

int
main ()
{
  try
    {
      throw A();		// ERROR - can't copy
    }
  catch (...) { }
}

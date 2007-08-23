// PR c++/31941
// { dg-do compile }

struct S
{
  S() throw () { }
  virtual ~S () throw ();
  virtual const char* what () const throw ();
};

const char *
foo (S &e)
{
  return e.what ().c_str ();	// { dg-error "c_str.*S::what.*which is of non-class type" }
}

// PR c++/33501
// { dg-do compile }

class A;	// { dg-error "forward declaration" }

template <typename T> struct X
{
  static int f (T);		// { dg-error "initializing" }
  static const T &make ();
  static const bool value = sizeof (f (make ())) == sizeof (int);	// { dg-error "invalid use of incomplete type" }
};

int
main ()
{
  return X <A>::value;
}

// PR c++/33501
// { dg-do compile }

class A;	// { dg-message "forward declaration" }

template <typename T> struct X
{
  static int f (T);		// { dg-message "initializing" }
  static const T &make ();
};

int
main ()
{
  return X<A>::f (X<A>::make ());	// { dg-error "invalid use of incomplete type" }
}

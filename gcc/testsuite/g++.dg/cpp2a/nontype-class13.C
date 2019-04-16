// PR c++/77304
// { dg-do compile { target c++2a } }

struct S {};

template < typename T > struct A
{
  template < S > void f () {}

  static void * g ()
  {
    return (void *) f < a >; // { dg-error "invalid" }
  }

  static S a;
};

void * f ()
{
  return A < int >::g ();
}

// PR c++/84082
// { dg-do compile }
// { dg-options "" }

struct A;

template<typename> void foo()
{
  static int a[A().operator=(A())];	// { dg-error "invalid use of incomplete type 'struct A'" }
}

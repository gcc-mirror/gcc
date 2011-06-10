// Test for doing the right thing with injected-class-name used as template
// type argument.  This is an extension from DR 176.

// { dg-options "-pedantic -std=c++98" }

template <class T>
struct A { };

template <template <class> class TTP>
struct B { };

struct C: A<int>
{
  B<A> b;			// { dg-warning "injected-class-name" }
};

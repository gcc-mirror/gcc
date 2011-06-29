// PR c++/14912
// Bug: We were instantiating A<B> in order to compare it to the matching
// argument for C<B,B>, which fails.

template <class T>
struct A
{
  typedef typename T::F F;
};

struct B { };

template <class T, class U = typename A<T>::F >
struct C
{
  typename T::F f;		// { dg-error "no type" }
};

C<B, B> c;			// { dg-message "required" }

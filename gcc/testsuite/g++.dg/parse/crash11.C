// { dg-do compile }

// Origin: priesnit@math.uni-goettingen.de

// PR c++/5767: ICE parsing typename with invalid scope.

template <template <typename> class TP>
struct A
{
  template <typename T>
  struct Template
  {
    typedef typename TP<T>::Type Type;
  };
};
template <template <typename> class TP>
struct B
{
  template <typename T>
  struct Template
  {
    typedef typename A<A<TP>::Template>  // { dg-error "mismatch|class template" }
      ::template Template<T>::Type Type; // { dg-error "unqualified-id" }
  };
};
template <typename T>
struct C
{
  typedef void Type;
};
int main()
{
  typedef B<C>::Template<void>::Type Type; // { dg-error "does not name a type" }
}

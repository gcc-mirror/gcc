// { dg-additional-options "-Wshadow=compatible-local" }

// pr c++/82424 we were trying to convert between dependent types.
template <typename T> class a
{
  struct b;
  template <typename, typename> void c ();
};
template <typename T>
template <typename, typename>
void
a<T>::c ()
{
  typedef typename T::b b; // Don't go looking inside the typename
  T thing;
  {
    T thing; // { dg-warning "shadows a previous local" }
  }
}


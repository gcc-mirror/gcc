// PR c++/70141

template <typename T>
struct outer
{
  template <typename U>
  struct inner
  {

  };
};


template <typename T>
struct is_inner_for
{
  template <typename Whatever>
  struct predicate;

  template <typename U>
  struct predicate<typename outer<T>::template inner<U> >
  {
  };
};

is_inner_for<int>::predicate<outer<int>::inner<double> > p;

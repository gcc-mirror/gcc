// PR c++/65370

template <typename> class C
{
  template <typename U>
  C(const C<U>&, bool = false);
};

template<>
class C<int>
{
  template <typename U>
  C(const C<U>&, bool);
};

template <typename U> C<int>::C(const C<U>&, bool = false) { }  // { dg-error "default arguments" }

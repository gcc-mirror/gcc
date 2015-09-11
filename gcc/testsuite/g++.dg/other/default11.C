// PR c++/65370

template <typename> class C
{
  template <typename U>
  C(const C<U>&, bool = false);
};

template <>
template <typename U>
C<int>::C(const C<U>&, bool);

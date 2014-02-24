// PR c++/37140

struct C
{
  static const int block_size = 1;
};

template <typename T> struct A {
  typedef C type;
};

template <typename T> struct B : public A<T> {
  using typename A<T>::type;
  static const int block_size = type::block_size;
};

template class B<int>;

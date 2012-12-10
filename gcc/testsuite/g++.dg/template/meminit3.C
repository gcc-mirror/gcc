// PR c++/54744

template <typename T>
struct base {
  typedef base base_type;
};

template <typename T>
struct derived : base<T> {
  typedef typename derived::base_type::base_type base_type;
  derived() : base_type() {}
};

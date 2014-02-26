// PR c++/58047

template <int N>
struct print_arg { };

struct const_holder {
  static const int CONSTANT = 42;
};

template <typename T>
struct identity {
  typedef T type;
};

template <class T>
struct test_case : public identity<T> {
  using typename identity<T>::type;
  print_arg<type::CONSTANT> printer;
};

template struct test_case<const_holder>;

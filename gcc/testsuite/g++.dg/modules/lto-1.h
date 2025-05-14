template <typename> struct S {
  S() {}
};
template <typename> inline int x = 0;

extern template struct S<char>;
extern template int x<char>;

template <typename> int* foo() {
  static int x;
  return &x;
};
extern template int* foo<char>();

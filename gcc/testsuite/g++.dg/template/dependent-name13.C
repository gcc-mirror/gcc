// DR 1710 - Missing template keyword in class-or-decltype

template<typename T> struct S {
  void fn(typename T::template B<int>::template C<int>);
  void fn2(typename T::B<int>::template C<int>);
  void fn3(typename T::template B<int>::C<int>);
  void fn4(typename T::B<int>::C<int>);
};

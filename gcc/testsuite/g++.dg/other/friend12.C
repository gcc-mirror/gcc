// PR c++/59480

template<typename>
class test {
  friend int foo(bool = true) { return 1; }  // { dg-message "14:previous" }
  friend int foo(bool);  // { dg-error "14:friend declaration" }
  template<typename> friend int bar(bool = true) { return 1; }  // { dg-message "33:previous" }
  template<typename> friend int bar(bool);  // { dg-error "33:friend declaration" }
};

template class test<bool>;

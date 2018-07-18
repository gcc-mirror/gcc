// PR c++/59480

class test {
  friend int foo(bool = true) { return 1; }  // { dg-message "14:previous" }
  template<typename> friend int bar(bool = true) { return 1; }  // { dg-message "33:previous" }
};

int foo(bool);  // { dg-error "5:friend declaration" }
template<typename> int bar(bool);  // { dg-error "24:friend declaration" }

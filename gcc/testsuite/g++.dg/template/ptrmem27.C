// PR c++/61500

struct X {
  int i;
  int j;

  int foo(int X::* ptr);

  template <int X::* ptr>
  int bar();
};

int X::foo(int X::* ptr) {
  int* p = &(this->*ptr);  // OK.
  return *p;
}

template <int X::* ptr>
int X::bar() {
  int* p = &(this->*ptr);  // gcc 4.9.0: OK in C++98 mode, fails in C++11 mode.
  return *p;
}

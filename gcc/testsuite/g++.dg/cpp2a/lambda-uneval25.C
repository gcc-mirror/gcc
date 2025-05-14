// { dg-do compile { target c++20 } }

template <class T>
void foo(T x) {
  sizeof []<int=0>(T=x) { return 0; }(); // { dg-error "may not appear" }
  sizeof [](T=x) { return 0; }(); // { dg-error "may not appear" }
};

void test() {
  foo(0);
}

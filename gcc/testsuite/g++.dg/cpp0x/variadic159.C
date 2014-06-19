// PR c++/61507
// { dg-do compile { target c++11 } }

struct A {
  void foo(const int &);
  void foo(float);
};

template <typename... Args>
void bar(void (A::*memfun)(Args...), Args... args);

void go(const int& i) {
  bar<const int &>(&A::foo, i);
}

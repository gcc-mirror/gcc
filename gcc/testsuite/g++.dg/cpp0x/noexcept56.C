// PR c++/92907 - noexcept does not consider "const" in member functions.
// { dg-do compile { target c++11 } }

void f(const int&);
void f(int&) = delete;

struct A {
  int i;
  void g() const noexcept(noexcept(f(i)));
};

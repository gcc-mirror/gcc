// PR c++/106201
// { dg-do compile { target c++11 } }

struct A {
  template<class T, class = decltype(f(*(T*)nullptr))>
  A(const T&);
};

struct B {
  template<class T> B(const T&);
};

void f(A&);
void f(B);

struct C { };

int main() {
  C c;
  f(c);
}

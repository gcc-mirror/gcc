// PR c++/80829
// { dg-do compile { target c++11 } }

struct A {
  constexpr A(int a) : _a(a) {}
  int _a;
};

struct B : public A {
  constexpr B(int a) : A(a) {}
};

int main() {
  constexpr A a = B(10);
}

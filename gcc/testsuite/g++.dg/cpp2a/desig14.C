// PR c++/89871
// { dg-do compile { target c++11 } }
// { dg-options "-Wall" }

struct A {};
struct B {};

struct S {
  union {
    A a;
    B b;
  };
};

int main() {
  S s;
  s = S{.a = A{}};
}

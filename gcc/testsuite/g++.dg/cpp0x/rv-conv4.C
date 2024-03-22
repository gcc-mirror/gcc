// PR c++/113064
// { dg-do compile { target c++11 } }

struct B { };

struct A {
  operator B() &;
  operator B&&() &&;
};

void f(B&&);

int main() {
  A a;
  f(a);
}

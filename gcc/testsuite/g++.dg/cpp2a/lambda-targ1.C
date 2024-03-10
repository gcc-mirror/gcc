// PR c++/103807
// { dg-do compile { target c++20 } }

template<auto = +[]{}>
struct A { };

A x;

int main() {
  A y;
}

// PR c++/105589
// { dg-do compile { target c++11 } }

struct X { X(); };

struct array { X m[2]; };

template<class>
void f() {
  array w = array{};
}

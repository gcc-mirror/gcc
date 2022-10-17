// { dg-do compile { target c++20 } }

struct empty { };

consteval void f(empty) { }

template<class>
void g(empty e) {
  f(e);
}

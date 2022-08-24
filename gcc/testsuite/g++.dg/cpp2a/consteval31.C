// PR c++/105912
// { dg-do compile { target c++20 } }

struct A {
  consteval A operator+() {
    return {};
  }
};

consteval A operator~(A) {
  return {};
}

consteval A operator+(A, A) {
  return {};
}

template<class>
void f() {
  A a;
  ~a;
  a + a;
  +a;
}

template void f<int>();

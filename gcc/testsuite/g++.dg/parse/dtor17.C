// PR c++/65091
// { dg-do compile { target c++11 } }

template<typename T>
auto foo(T x) -> decltype(~x) {
  return ~x;
}

int bar() {
  return foo(10);
}

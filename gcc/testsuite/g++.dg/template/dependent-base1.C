// PR c++/71209

struct A {
  int table_clear;
};

template <typename T>
struct B : T {
  B() { this->A::table_clear; }
};

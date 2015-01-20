// PR c++/62276
// { dg-do compile { target c++11 } }

template <typename T> using bar = T;

template <template <typename...> class F = bar>
void foo() {}

int main() { foo(); }

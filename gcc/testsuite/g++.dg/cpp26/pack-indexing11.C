// P2662R3 - Pack Indexing
// PR c++/113798
// { dg-do compile { target c++26 } }
// Test case from [diff.cpp23.dcl.dcl].

template <typename... T>
void f(T... [1]);
template <typename... T>
void g(T... ptr[1]);
int main() {
  f<int, double>(nullptr, nullptr);     // { dg-error "no matching function" }
  g<int, double>(nullptr, nullptr);     // ok
}

// PR c++/67003
// { dg-options "-std=c++17 -fconcepts" }

namespace X {
  template<class>
  concept bool C = true;
}

X::C{T}
void foo() {}

int main() { foo<int>(); }

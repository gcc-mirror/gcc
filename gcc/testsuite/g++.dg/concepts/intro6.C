// PR c++/67003
// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

namespace X {
  template<class>
  concept bool C = true;
}

X::C{T}
void foo() {}

int main() { foo<int>(); }

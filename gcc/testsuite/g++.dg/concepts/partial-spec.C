// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

// Check that constraints don't break unconstrained partial
// specializations.

template<typename T>
  struct S { };

template<typename T>
  struct S<T*> { };

template<>
  struct S<int> { };

int main() { }

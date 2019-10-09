// { dg-do compile { target c++17_only } }
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

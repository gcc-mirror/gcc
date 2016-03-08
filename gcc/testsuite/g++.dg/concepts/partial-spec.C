// { dg-options "-std=c++1z -fconcepts" }

// Check that constraints don't break unconstrained partial
// specializations.

template<typename T>
  struct S { };

template<typename T>
  struct S<T*> { };

template<>
  struct S<int> { };

int main() { }

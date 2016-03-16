// PR c++/58281 - Problem with explicitly instantiated constexpr template
//     functions 
// { dg-do compile { target c++11 } }
// { do-additional-options "-fdump-tree-optimized" }


template <typename T>
constexpr bool f (T a)
{
  return a == 3;
}

extern template bool f<int>(int);

bool g (int x) { return f (x); }

template bool f<int>(int);

// Verify that the defintions of both f() and g() are emitted.
// { dg-final { scan-tree-dump "bool \[fg\](" "optimized" } }

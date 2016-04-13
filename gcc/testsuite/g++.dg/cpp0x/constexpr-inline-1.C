// PR c++/53792 - [C++11] improving compiler-time constexpr evaluation
//     Test case from comment #8.
// { dg-do compile { target c++14 } }
// { dg-additional-options "-O1 -fdump-tree-optimized" }

template <class T>
void sink (T);
  
constexpr unsigned foo ()
{
  unsigned  i = 1;
  while ((i << 1) > i)
    i = i << 1;

  return i;
}

template <unsigned N>
struct S { };

void bar ()
{
  sink (foo ());
  sink (S<foo ()>());
}

// Verify that the call to the foo() constexpr function is inlined
// regardless of whether or not it's invoked in a constant expression.
// { dg-final { scan-tree-dump-not "= *foo *\\\(" "optimized" } }

// PR c++/115657
// { dg-do compile { target c++14 } }
// { dg-options "-Wall" }

// Like constexpr-recursion1.C but use a class with a conversion function.

struct X {
  constexpr operator int() { return 0; }
};

template <int N>
constexpr X f1 ()
{
  enum E { a = f1<0> () }; // { dg-error "called in a constant expression before its definition is complete|is not an integer constant" }
  return {};
}

constexpr X f3 ()
{
  enum E { a = f3 () };	// { dg-error "called in a constant expression before its definition is complete|is not an integer constant" }
  return {};
}

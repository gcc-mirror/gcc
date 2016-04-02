// PR c++/70449
// { dg-do compile { target c++14 } }
// { dg-options "-Wall" }

template <int N>
constexpr int f1 ()
{
  enum E { a = f1<0> () }; // { dg-error "called in a constant expression before its definition is complete|is not an integer constant" }
  return 0;
}

constexpr int f3 ()
{
  enum E { a = f3 () };	// { dg-error "called in a constant expression before its definition is complete|is not an integer constant" }
  return 0;
}

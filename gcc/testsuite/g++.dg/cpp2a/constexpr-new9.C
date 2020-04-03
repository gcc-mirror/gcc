// PR c++/91369
// { dg-do compile { target c++2a } }
// { dg-additional-options "-fdelete-null-pointer-checks" }

struct S {
  constexpr S (int *i) : i{i} {}
  constexpr ~S () { delete[] i; }
  int *i;
};

constexpr S foo (int x) { return { new int[x] () }; }
constexpr bool bar () { foo (1); return true; }
constexpr bool baz () { foo (1); return false; }

static_assert (bar ());
static_assert (!baz ());

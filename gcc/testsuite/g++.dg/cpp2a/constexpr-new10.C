// PR c++/91369
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fdelete-null-pointer-checks" }

struct S {
  constexpr S (int* i) : s{i} {}
  constexpr ~S () { delete s; }
  int *s;
};

struct T { S t = { new int }; };

constexpr auto
foo ()
{
  T b;
  return true;
}

static_assert (foo ());

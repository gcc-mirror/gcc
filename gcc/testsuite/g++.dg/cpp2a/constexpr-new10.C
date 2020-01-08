// PR c++/91369
// { dg-do compile { target c++2a } }

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

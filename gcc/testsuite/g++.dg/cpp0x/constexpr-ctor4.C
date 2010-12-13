// PR c++/46873
// { dg-options -std=c++0x }

struct S
{
  int i:1;
};

struct T
{
  const S s;
  constexpr T (S a = S ()) : s (a) { }
};

T t;

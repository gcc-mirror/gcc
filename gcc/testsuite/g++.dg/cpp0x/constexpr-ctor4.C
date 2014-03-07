// PR c++/46873
// { dg-do compile { target c++11 } }

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

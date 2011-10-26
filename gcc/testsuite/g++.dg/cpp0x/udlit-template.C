// { dg-do run }
// { dg-options "-std=c++0x" }

// Test user-defined literals.
// Test template operator declaration and definition.

#include <cassert>

template<char...>
  int operator"" _abc();

template<>
  int
  operator"" _abc<>()
  { return -1; }

template<>
  int
  operator"" _abc<'L','U','E'>()
  { return 42; }

template<>
  int
  operator"" _abc<'6','6','6'>()
  { return 21; }

int
test1()
{
  int i = operator"" _abc<'1','2','3'>();
  assert(i == 45);
  int universal_meaning = operator"" _abc<'L','U','E'>();
  assert(universal_meaning == 42);
  int b = operator"" _abc<'6','6','6'>();
  int z = operator"" _abc<>();
  assert(z == -1);
  int j = 123_abc;
  assert(j == i);
  int jb = 666_abc;
  assert(jb == b);
}

int
main()
{
  test1();
}

template<char... Chars>
  int operator"" _abc()
  { return 42 + sizeof...(Chars); }

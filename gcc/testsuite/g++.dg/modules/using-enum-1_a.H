// { dg-additional-options {-fmodule-header -std=c++2a} }
// { dg-module-cmi {} }

enum class E {a, b, c};
struct C
{
  using enum E;
};

struct D: C 
{
  int foo ()
  {
    return int (a);
  }
};

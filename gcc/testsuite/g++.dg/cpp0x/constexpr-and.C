// PR c++/56481
// Non-linearity in potential_constant_expression_1
// { dg-options -std=c++11 }

struct S
{
  constexpr bool foo ();
#define A(n) , f##n##0, f##n##1, f##n##2, f##n##3
#define B(n) A(n##0) A(n##1) A(n##2) A(n##3)
#define C B(0) B(1) B(2) B(3)
  bool f C;
};

constexpr bool
S::foo ()
{
#undef A
#define A(n) && f##n##0 && f##n##1 && f##n##2 && f##n##3
  return f C;
}

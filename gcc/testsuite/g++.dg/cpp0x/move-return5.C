// { dg-do compile { target c++11 } }
// This used to compile in C++11...17 because we performed two
// separate overload resolutions: one treating the operand as
// an rvalue, and then (if that resolution fails) another one
// treating the operand as an lvalue.

struct W {
  W();
};

struct F {
  F(W&);
  F(W&&) = delete;
};

F fn ()
{
  W w;
  return w; // { dg-error "use of deleted function" }
}

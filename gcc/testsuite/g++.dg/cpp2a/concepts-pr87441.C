// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts" }

template<typename X, typename Y = X>
concept HasBinaryAdd = requires(X x, Y y)
{
  {x + y} -> decltype(x + y); // { dg-error "return-type-requirement is not a type-constraint" }
};

void proc(HasBinaryAdd auto x, HasBinaryAdd auto y);

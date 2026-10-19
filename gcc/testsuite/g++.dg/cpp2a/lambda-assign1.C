// PR c++/127710
// { dg-do compile { target c++20 } }

auto l1 = [] {} = {};
void
g ()
{
  auto l2 = [] {} = {};
  // [expr.prim.lambda.closure]/17: The closure type associated with
  // a lambda-expression [...] has a deleted copy assignment operator
  // if the lambda-expression has a lambda-capture.
  int i = 9;
  auto l3 = [i] {} = {};  // { dg-error "use of deleted function" }
}

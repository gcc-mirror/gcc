// PR c++/84171
// { dg-options "-Wsign-compare" }

bool foo (char c)
{
  const int i = 0 = 0; // { dg-error "lvalue" }
  return c = i;
} // { dg-warning "control reaches" }

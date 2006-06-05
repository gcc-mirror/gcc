// PR c++/27804
// { dg-do compile }

bool foo()
{
  const int i = X;  // { dg-error "not declared" }
  return i > 0;
}

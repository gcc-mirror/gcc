// { dg-do compile { target c++14 } }

void f()
{
  [&x=1]{};   // { dg-error "cannot capture|cannot bind non-const lvalue ref" }
}

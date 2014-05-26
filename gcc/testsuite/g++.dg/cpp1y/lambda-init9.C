// { dg-do compile { target c++1y } }

void f()
{
  [&x=1]{};   // { dg-error "cannot capture|invalid initialization" }
}

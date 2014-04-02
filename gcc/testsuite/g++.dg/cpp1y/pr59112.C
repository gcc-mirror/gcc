// PR c++/59112
// { dg-do compile { target c++1y } }

void foo()
{
  struct A
  {
    A(auto) {} // { dg-error "auto|not permitted" }
  };
}

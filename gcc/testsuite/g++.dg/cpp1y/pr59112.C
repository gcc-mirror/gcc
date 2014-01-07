// PR c++/59112
// { dg-do compile }
// { dg-options "-std=gnu++1y" }

void foo()
{
  struct A
  {
    A(auto) {} // { dg-error "auto|not permitted" }
  };
}

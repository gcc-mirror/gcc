// { dg-do compile }
// { dg-options "-std=gnu++1y" }

// PR c++/59112

void foo()
{
  struct A
  {
    A(auto) {} // { dg-error "auto|not permitted" }
  };
}

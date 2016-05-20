// PR c++/70572
// { dg-do compile { target c++14 } }

void foo ()
{
  decltype (auto) a = foo;  // { dg-error "initializer" }
}

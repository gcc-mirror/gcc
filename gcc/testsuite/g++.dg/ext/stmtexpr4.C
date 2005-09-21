// PR c++/20147
// { dg-do compile }
// { dg-options "" }

void foo()
{
  ({x;}); // { dg-error "was not declared" }
}

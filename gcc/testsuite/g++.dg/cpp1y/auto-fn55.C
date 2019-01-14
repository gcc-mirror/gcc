// PR c++/88825
// { dg-do compile { target c++14 } }

auto f () -> auto *
{
  int t = 0;
  return t; // { dg-error "unable to deduce" }
}

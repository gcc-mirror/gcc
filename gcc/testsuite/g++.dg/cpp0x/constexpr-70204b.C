// PR c++/70204
// { dg-do compile { target c++11 } }

int a;

void fn1 ()
{
  const int x = 0 * a;
  constexpr int y = x; // { dg-error "constant" }
}

// DR 1207
// PR c++/52869
// { dg-do compile { target c++11 } }

void
fn ()
{
  struct S {
    bool operator!() noexcept(false);
  } s;
  S t = s;
}

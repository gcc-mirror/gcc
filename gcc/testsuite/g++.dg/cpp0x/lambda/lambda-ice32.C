// PR c++/60366
// { dg-do compile { target c++11 } }

void
fn ()
{
  auto f = [](const struct __lambda0 &self) { self(self); }; // { dg-error "" }
  f(f); // { dg-error "" }
}

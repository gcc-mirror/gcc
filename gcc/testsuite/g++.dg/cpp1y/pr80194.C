// PR c++/80194
// { dg-do compile { target c++14 } }

int fn1 ();

template <class Fn>
void
fn2 (Fn &&fn)
{
  fn (42);
}

void fn2 ()
{
  auto const x = fn1 ();
  fn2 ([&](auto) { x; });
}

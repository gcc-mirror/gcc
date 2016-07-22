// PR c++/70540
// { dg-do compile { target c++11 } }

void
foo ()
{
  auto f = [&] { return f; };  // { dg-error "before deduction" }
}

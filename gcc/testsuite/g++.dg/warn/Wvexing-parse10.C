// PR c++/118718
// { dg-do compile { target c++14 } }

void
fn ()
{
  auto f1 () -> auto;
  auto f2 (); // { dg-warning "empty parentheses" }
}

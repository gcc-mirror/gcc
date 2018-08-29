// PR c++/85977, Incorrect handling of array reference size deduction
// { dg-do compile { target c++11 } }

template <int N>
void fn (const char (&)[N]) { }

void
bar ()
{
  fn ({1.2}); // { dg-error "narrowing" }
}

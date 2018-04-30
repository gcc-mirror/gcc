// { dg-do compile { target c++11 } }
// { dg-additional-options "-O" }
// pr 82878 erroneously unwrapped a reference parm in the lambda::_FUN
// thunk.

struct A {
  ~A();
  operator int ();
};

void baz ();

void
bar (A b)
{
  void (*lam) (A) = [](A) { baz (); };

  if (auto c = b)
    lam (c);
}

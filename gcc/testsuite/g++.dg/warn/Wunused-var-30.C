// PR c++/84488
// { dg-do compile }
// { dg-options "-Wunused-but-set-variable" }

int
foo ()
{
  enum E { A, B, C, D };
  double r = 1.0;		// { dg-bogus "set but not used" }
  return static_cast<E>(r);
}

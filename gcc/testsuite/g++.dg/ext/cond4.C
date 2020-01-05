// PR c++/93046
// { dg-do compile }
// { dg-options "" }

struct S {
  S (int);
  operator bool ();
};

S
foo ()
{
  return S (1) ? : S (2);
}

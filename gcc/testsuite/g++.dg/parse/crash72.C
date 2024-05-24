// PR c++/105229
// { dg-do compile { target c++20 } }
// { dg-additional-options "-Wno-missing-template-keyword" }

template <typename> void bar ()
{
  [] <int> {}.operator () <> (); // { dg-error "expected primary-expression" }
}
void foo ()
{
  bar<int> ();
}

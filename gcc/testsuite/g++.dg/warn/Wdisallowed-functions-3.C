// PR c++/39554
// { dg-do compile }
// { dg-options "-Wdisallowed-function-list=bar" }

void
foo (void (*p) (), void (*bar) ())
{
  p ();
  bar ();
}

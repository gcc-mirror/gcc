// PR c++/12795
// { dg-do compile { target c++11 } }
// { dg-require-alias "" }

void foo()
{
  extern void bar [[gnu::__alias__ ("BAR")]] (); // { dg-warning "ignored" }
  bar ();
}

// PR c++/12795
// { dg-require-alias "" }

void foo()
{
  extern void bar () __attribute__ ((__alias__ ("BAR"))); // { dg-warning "ignored" }
  bar ();
}

// PR c++/12795

void foo()
{
  extern void bar () __attribute__ ((__alias__ ("BAR")));
  bar ();
}

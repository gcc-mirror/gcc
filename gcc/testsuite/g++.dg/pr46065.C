// PR c++/46065
// { dg-do compile }

void bar ();

void
foo ()
{
  using ::bar;
label:;
}

// PR c++/89767
// { dg-do compile { target c++14 } }

void bar (int);

void
foo ()
{
  int x = 0;
  auto z = [x, y = [x] { bar (x); }] { y (); bar (x); };
  z ();
}

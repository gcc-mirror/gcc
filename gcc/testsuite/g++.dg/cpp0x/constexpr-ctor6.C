// PR c++/47041
// { dg-options "-std=c++11 -fno-elide-constructors" }

struct S
{
  int i;
};

S s = S ();

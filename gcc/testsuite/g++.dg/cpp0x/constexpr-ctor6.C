// PR c++/47041
// { dg-options "-std=c++0x -fno-elide-constructors" }

struct S
{
  int i;
};

S s = S ();

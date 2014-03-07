// PR c++/47041
// { dg-do compile { target c++11 } }
// { dg-options "-fno-elide-constructors" }

struct S
{
  int i;
};

S s = S ();

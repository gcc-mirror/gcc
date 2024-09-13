// PR c++/116678
// { dg-do compile { target c++11 } }
// { dg-options "-Os -pedantic" }

struct S
{
  [[deprecated]] S () { s = 1; }	// { dg-bogus "'S::S\\\(\\\)' is deprecated" }
  S (int x) { s = x; }
  ~S () {}
  int s;
};

int
main ()
{
}

// PR c++/116678
// { dg-do compile }
// { dg-options "-Os -pedantic" }

struct S
{
  [[deprecated]] S () { s = 1; }	// { dg-bogus "'S::S\\\(\\\)' is deprecated" }
  S (int x) { s = x; }			// { dg-warning "C\\\+\\\+11 attributes only available with" "" { target c++98_only } .-1 }
  ~S () {}
  int s;
};

int
main ()
{
}

// PR c++/10929
// { dg-options "-Winline -O3" }

int foo ();
int bar () { return foo (); }

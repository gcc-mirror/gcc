// PR c++/54844
// { dg-do compile }
template <int N> int fn () { return sizeof (double); }
int var = fn <0> ();

//Origin: bangerth@dealii.org
//PR c++/11490
//Since N is know at instantiation time, there
// should be no warning about comparision between
// unsinged and signed interegers.

// { dg-do compile }
// { dg-options "-W" }

template <int N> bool f() {
  unsigned int i=0;
  return i!=N;  // { dg-bogus "signed and unsigned" }
}

template bool f<2> ();

// Build don't link:
// Special g++ Options:
// Origin: Mark Mitchell <mark@codesourcery.com>

template <double d> // WARNING - deprecated
struct S;

template <double d, double e> // WARNING - deprecated
void f (S<d>*, S<e>*, S<d + e>*);  // WARNING - deprecated

void g ()
{
  S<2.0>* s1; // WARNING - deprecated
  S<3.7>* s2; // WARNING - deprecated
  S<5.7>* s3; // WARNING - deprecated
  
  f (s1, s2, s3);
}


// Build don't link:
// Special g++ Options:
// Origin: Mark Mitchell <mark@codesourcery.com>

template <double d>
struct S;

template <double d, double e>
void f (S<d>*, S<e>*, S<d + e>*);

void g ()
{
  S<2.0>* s1;
  S<3.7>* s2;
  S<5.7>* s3;
  
  f (s1, s2, s3);
}


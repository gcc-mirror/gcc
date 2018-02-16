// { dg-do assemble  }
// { dg-options "-ffriend-injection" }
// Bug: g++ ignores the :: qualification and dies trying to treat an integer
// variable as a list of functions.

class DComplex;
double imag (const DComplex&);

class DComplex {
public:
  friend  double   imag(const DComplex& a); // Not injected, no warning
};

class FComplex {
public:
  friend  float    imag(const FComplex& a); // { dg-warning "is visible" }
};

void
scnrm2(FComplex cx[])
{
  int imag;
  ::imag( cx[0] );
}
// { dg-warning "ffriend-injection.* is deprecated" "cc1plus:" { target *-*-* } 0 }

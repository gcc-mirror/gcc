// Bug: g++ ignores the :: qualification and dies trying to treat an integer
// variable as a list of functions.
// Build don't link:

class DComplex {
public:
  friend  double   imag(const DComplex& a);
};

class FComplex {
public:
  friend  float    imag(const FComplex& a);
};

void
scnrm2(FComplex cx[])
{
  int imag;
  ::imag( cx[0] );
}

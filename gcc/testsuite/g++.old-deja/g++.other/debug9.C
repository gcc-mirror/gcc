// Build don't link:
// Special g++ Options: -g

//  Copyright (C) 2001 Free Software Foundation, Inc.
//  Contributed by Jeffrey D. Oldham 2001 May 17 <oldham@codesourcery.com>.

// This illustrates that debugging information for CONCAT RTL's
// supports only COMPLEX_TYPE types, not other types.

namespace std {
  template<typename _Tp> class complex;

  template<> class complex<double>
  {
  public:
    complex(double =0.0, double =0.0);

  private:
    typedef __complex__ double _ComplexT;
    _ComplexT _M_value;
  };

  inline
  complex<double>::complex(double __r, double __i)
  {
    __real__ _M_value = __r;
    __imag__ _M_value = __i;
  }
}

template <int Dim, class T>
class Engine
{
public:
  Engine (T val = T()) {}
};

int main()
{
  Engine<1, std::complex<double> > e;
  return 0;
}

// { dg-do compile { target c++14 } }
using namespace std::complex_literals; // { dg-error "" }
// { dg-message "#include <complex>" "" { target *-*-* } .-1 }

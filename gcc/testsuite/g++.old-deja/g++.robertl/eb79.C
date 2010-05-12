// { dg-do assemble  }
// { dg-prune-output "note" }
// Makes bogus x86 assembly code.
#include <iostream>

// The VxWorks kernel-mode headers define a macro named "max", which is not
// ISO-compliant, but is part of the VxWorks API.
#if defined __vxworks && !defined __RTP__
#undef max
#endif

using namespace std;

template<class T>
T max(T a, T b)
{
  return (a > b) ? a : b;
}

// Prototypes (enable one or the other)
double max<>(double, double);      // { dg-error "" } bogus code
// int max(int, int);

int main()
{
  int i = 123;
  double d = 1234.5678;

  cout.precision(12);
  cout << max(d, i) << endl;  // { dg-error "" } 
  cout << max(i, d) << endl;  // { dg-error "" } 
  return 0;
}

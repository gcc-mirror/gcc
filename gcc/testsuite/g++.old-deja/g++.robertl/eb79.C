// Makes bogus x86 assembly code.
#include <iostream.h>

template<class T>
T max(T a, T b)
{
  return (a > b) ? a : b;
}

// Prototypes (enable one or the other)
double max<>(double, double);      // ERROR - bogus code
// int max(int, int);

int main()
{
  int i = 123;
  double d = 1234.5678;

  cout.precision(12);
  cout << max(d, i) << endl;  // #1
  cout << max(i, d) << endl;  // #2
  return 0;
}

// GROUPS passed templates
// Test that the compiler will emit the definition of min given just
// the declaration.  At one point in the past, it did not.
// Special g++ Options: -fguiding-decls
#include <iostream.h>

template <class T> inline T min(T a, T b) { return (a < b) ? a : b;};

double min(double,double);	// this should expand the template

main () {
    int a = 1, b = 2;
    double aa = 2.0, bb = 1.2;
    cout << min(a,b) << ", " << min(aa,bb) << "\n";
    if (min (a, aa) == a)
      cout << "PASS" << endl;
    else
      cout << "FAIL" << endl;
}


#include <complex>

void foo() {
    complex<double> x(0, 0);
    complex<double> y = 1.0 + x;   // OK
    complex<double> z = 1 + x;     // line 6: <<<< 1 doesn't match double
}

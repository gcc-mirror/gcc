/* Derived from PR optimization/11311 */

double pow(double, double);

double foo(double x) { return pow(x,261); }


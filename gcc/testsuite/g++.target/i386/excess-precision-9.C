// Excess precision tests.  Test implicit conversions in comparisons:
// no excess precision in C++.
// { dg-do run }
// { dg-options "-mfpmath=387 -fexcess-precision=standard" }

#include "../../gcc.target/i386/excess-precision-9.c"

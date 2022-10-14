// Excess precision tests.  Test C++ semantics for conversions from
// integers to floating point: no excess precision for either explicit
// or implicit conversions.
// { dg-do run }
// { dg-options "-mfpmath=387 -fexcess-precision=standard" }

#include "../../gcc.target/i386/excess-precision-8.c"

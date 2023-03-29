// Excess precision tests.  Test that excess precision is carried
// through various operations.
// { dg-do run }
// { dg-options "-O2 -mfpmath=387 -fexcess-precision=standard" }

#include "../../gcc.target/i386/excess-precision-1.c"

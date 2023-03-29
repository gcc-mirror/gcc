// Excess precision tests.  Test excess precision is removed when
// necessary.
// { dg-do run }
// { dg-options "-O2 -mfpmath=387 -fexcess-precision=standard" }

#include "../../gcc.target/i386/excess-precision-3.c"

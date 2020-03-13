// { dg-additional-options "-fmodules-ts -fno-module-lazy" }
// FIXME: Not an ODR violation!
#include "lambda-2.h"
import "lambda-2_a.H";

// { dg-excess-errors "not an odr violation" { xfail *-*-* } }

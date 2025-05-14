// { dg-additional-options "-fmodules-ts -fno-module-lazy -Wno-subobject-linkage" }
// { dg-xfail-if "see PR c++/118245" { c++20 } }
// Test for ODR deduplication

#include "lambda-7.h"
import "lambda-7_a.H";

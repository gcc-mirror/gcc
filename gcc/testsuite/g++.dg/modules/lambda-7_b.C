// { dg-additional-options "-fmodules-ts -fno-module-lazy -Wno-subobject-linkage" }
// Test for ODR deduplication

#define CHECK_ODR_VIOLATIONS
#include "lambda-7.h"
import "lambda-7_a.H";

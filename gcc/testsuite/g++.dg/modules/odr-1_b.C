// { dg-additional-options "-fmodules-ts -fno-module-lazy" }

#include "odr-1.h"
import "odr-1_a.H";

// { dg-regexp {[^\n]*odr-1.h:3:[0-9]*: error: conflicting global module declaration .constexpr const<lambda\(\)> all.\n} }
// { dg-regexp {[^\n]*note: existing declaration .constexpr const<lambda\(\)> all.\n} }

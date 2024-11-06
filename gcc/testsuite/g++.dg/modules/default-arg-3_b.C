// PR c++/99274
// { dg-additional-options "-fmodules-ts -fno-module-lazy" }
// Don't complain about matching defaults.

#include "default-arg-3.h"
import "default-arg-3_a.H";

// { dg-additional-options "-fmodules -std=c++20 -fno-module-lazy" }

#include "lambda-9.h"
import "lambda-9_a.H";

static_assert(C<int>);

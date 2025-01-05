// PR c++/116568
// { dg-additional-options "-fmodules-ts -fno-module-lazy -std=c++20" }

#include "lambda-8.h"
import "lambda-8_a.H";

// { dg-error "conflicting global module declaration" "" { target *-*-* } 0 }

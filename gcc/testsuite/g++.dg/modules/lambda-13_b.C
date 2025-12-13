// PR c++/123075
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fmodules -fno-module-lazy" }

#include "lambda-13.h"
import "lambda-13_a.H";

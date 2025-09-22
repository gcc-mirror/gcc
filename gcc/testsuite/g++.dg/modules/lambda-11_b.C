// PR c++/122015
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fmodules -fno-module-lazy" }

#include "lambda-11.h"
import "lambda-11_a.H";

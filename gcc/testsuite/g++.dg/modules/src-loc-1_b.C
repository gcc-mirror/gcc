// PR c++/118904
// { dg-additional-options "-fmodules -std=c++20 -fno-module-lazy" }

#include "src-loc-1.h"
import "src-loc-1_a.H";

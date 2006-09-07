// PR c++/26195
// { dg-do link }
// { dg-additional-sources "interface3a2.cc" }
// { dg-options "-I. -fno-inline" }

#pragma implementation "dir1/interface3.cc"
#include "dir1/interface3.h"

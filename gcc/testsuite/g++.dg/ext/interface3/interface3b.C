// PR c++/26195
// { dg-do link }
// { dg-additional-sources "interface3b2.cc" }
// { dg-options "-fno-inline" }

#pragma implementation "dir2/interface3.cc"
#include "dir1/interface3.h"

// PR c++/69145
// { dg-do compile }
#pragma implementation "pr69145-2-very-long-filename.cc" // { dg-bogus "appears after file is included" }
#include "pr69145-2-very-long-filename.cc"

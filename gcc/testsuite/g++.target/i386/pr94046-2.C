// PR target/94046
// { dg-do compile }
// { dg-options "-O0 -mavx2 -mxop" }
// { dg-skip-if "requires hosted libstdc++ for cstdlib malloc" { ! hostedlib } }

#include "pr94046-1.C"

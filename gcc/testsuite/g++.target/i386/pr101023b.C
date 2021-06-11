// PR target/101023
// { dg-do run { target { ! ia32 } } }
// { dg-options "-O2 -mno-red-zone -mtune=opteron -mstackrealign --param=hot-bb-frequency-fraction=1" }

#include "pr101023a.C"

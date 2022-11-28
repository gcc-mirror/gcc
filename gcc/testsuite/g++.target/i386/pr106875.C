// PR target/106875
// { dg-do compile { target { c++11 && lp64 } } }
// { dg-options "-O0 -mabi=ms -fabi-version=3 -mcall-ms2sysv-xlogues" }

#include "pr101180.C"

// PR tree-optimization/60559
// { dg-do compile }
// { dg-additional-options "-O3 -std=c++11 -fnon-call-exceptions -fno-tree-dce" }
// { dg-additional-options "-mavx2" { target { i?86-*-* x86_64-*-* } } }

#include "pr60023.cc"

// { dg-final { cleanup-tree-dump "vect" } }

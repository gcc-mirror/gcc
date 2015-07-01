// { dg-options "-std=c++11" }
// { dg-do run }

typedef unsigned short V __attribute__((vector_size(32)));
typedef V VI;

#include "vshuf-16.inc"
#include "vshuf-main.inc"

// { dg-options "-std=c++11" }
// { dg-do run }

typedef unsigned char V __attribute__((vector_size(16)));
typedef V VI;

#include "vshuf-16.inc"
#include "vshuf-main.inc"

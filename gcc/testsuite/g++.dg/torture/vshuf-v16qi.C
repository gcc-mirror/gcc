// { dg-do run { target c++11 } }

typedef unsigned char V __attribute__((vector_size(16)));
typedef V VI;

#include "vshuf-16.inc"
#include "vshuf-main.inc"

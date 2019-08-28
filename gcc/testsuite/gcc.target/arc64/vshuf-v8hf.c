/* { dg-do run } */
/* { dg-require-effective-target dpfp } */
/* { dg-options "-DEXPENSIVE -O2" } */

typedef _Float16 V __attribute__((vector_size(16)));
typedef unsigned short VI __attribute__((vector_size(16)));

#include "vshuf-8.inc"
#include "vshuf-main.inc"

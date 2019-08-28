/* { dg-do run } */
/* { dg-require-effective-target dpfp } */
/* { dg-options "-DEXPENSIVE -O2" } */

typedef _Float16 V __attribute__((vector_size(8)));
typedef unsigned short VI __attribute__((vector_size(8)));

#include "vshuf-4.inc"
#include "vshuf-main.inc"

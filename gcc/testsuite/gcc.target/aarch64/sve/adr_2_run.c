/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#define FACTOR 4
#include "adr_1_run.c"

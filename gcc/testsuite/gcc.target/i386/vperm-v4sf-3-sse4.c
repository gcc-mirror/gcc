/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O -msse4.1" } */
#include "isa-check.h"
#define CHECK_ISA
#include "vperm-v4sf-3.c"

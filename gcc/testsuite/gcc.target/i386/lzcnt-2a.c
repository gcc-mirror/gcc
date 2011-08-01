/* { dg-do compile } */
/* { dg-options "-O2 -mlzcnt" } */

#include "lzcnt-2.c"

/* { dg-final { scan-assembler "lzcntw" } } */

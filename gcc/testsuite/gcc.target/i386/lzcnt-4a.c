/* { dg-do compile } */
/* { dg-options "-O2 -mlzcnt" } */

#include "lzcnt-4.c"

/* { dg-final { scan-assembler "lzcntl" } } */

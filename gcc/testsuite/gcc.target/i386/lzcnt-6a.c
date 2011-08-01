/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mlzcnt" } */

#include "lzcnt-6.c"

/* { dg-final { scan-assembler "lzcntq" } } */

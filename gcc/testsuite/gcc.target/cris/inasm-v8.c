/* { dg-do assemble } */
/* { dg-options "-DOTHER_ISA=8 -march=v8" { target { ! march_option } } } */

/* Check that -march=v8 is also recognized.  */

#include "inasm-other.c"

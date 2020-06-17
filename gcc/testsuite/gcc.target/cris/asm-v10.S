/* { dg-do assemble } */
/* { dg-options "-DOTHER_ISA=10 -march=v10" { target { ! march_option } } } */

/* Check that -march=v10 is also recognized.  */

#include "asm-other.S"

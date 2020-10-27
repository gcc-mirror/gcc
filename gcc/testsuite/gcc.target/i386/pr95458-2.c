/* { dg-do compile } */
/* { dg-options "-O2 -mno-inline-all-stringops" } */

#include "pr95458-1.c"

/* { dg-final { scan-assembler "call\[\\t \]*_?strncmp" } } */
/* { dg-final { scan-assembler-not "cmpsb" } } */

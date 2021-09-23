/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-mavx -O2 -mabi=ms" } */
/* { dg-final { scan-assembler-not {(?n)xmm([6-9]|1[0-5])} } } */

#include "pr82735-2.c"

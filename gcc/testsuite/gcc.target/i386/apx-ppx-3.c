/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mapx-features=ppx" } */

/* { dg-final { scan-assembler-not "pushp" } } */
/* { dg-final { scan-assembler-not "popp" } } */

#include "eh_return-2.c"

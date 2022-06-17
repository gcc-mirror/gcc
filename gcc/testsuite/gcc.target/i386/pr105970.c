/* PR target/105970 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-mx32 -mgeneral-regs-only -maddress-mode=long" } */

#include "../../gcc.dg/torture/pr68037-1.c"

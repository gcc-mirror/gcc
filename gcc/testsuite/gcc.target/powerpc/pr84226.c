/* PR target/84226 */
/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-options "-mvsx -mpower9-misc -O1" } */
/* { dg-require-effective-target powerpc_vsx } */

#include "builtins-revb-runnable.c"

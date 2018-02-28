/* PR target/84226 */
/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mpower9-misc -O1" } */

#include "builtins-revb-runnable.c"

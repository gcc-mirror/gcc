/* PR c/102989 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23" } */

#include <limits.h>

#ifndef BITINT_MAXWIDTH
#error BITINT_MAXWIDTH not defined
#elif BITINT_MAXWIDTH < ULLONG_WIDTH
#error BITINT_MAXWIDTH smaller than ULLONG_WIDTH
#endif

_BitInt(BITINT_MAXWIDTH) a;
_BitInt(BITINT_MAXWIDTH + 1) b;		/* { dg-error "'_BitInt' argument '\[0-9]+' is larger than 'BITINT_MAXWIDTH' '\[0-9]+'" } */

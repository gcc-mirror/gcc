/* PR c/102989 */
/* { dg-do compile } */
/* { dg-options "-std=c17" } */

#include <limits.h>

#ifdef BITINT_MAXWIDTH
#error BITINT_MAXWIDTH defined in C11
#endif

/* Test for cross x86_64<->w64 abi standard calls.
*/
/* { dg-do compile { target { x86_64-*-* } } } */
/* { dg-options "-mno-sse" } */
#include "callabi.h"

long double
CALLABI_CROSS func_cross ()
{
}

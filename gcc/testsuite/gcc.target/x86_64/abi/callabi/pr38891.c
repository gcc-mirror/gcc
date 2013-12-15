/* Test for cross x86_64<->w64 abi standard calls.
*/
/* { dg-do compile } */
/* { dg-options "-mno-sse -maccumulate-outgoing-args" } */
#include "callabi.h"

long double
CALLABI_CROSS func_cross ()
{
}

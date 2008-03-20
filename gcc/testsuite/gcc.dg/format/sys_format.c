/* Test system default printf formatter specifiers.  */
/* Origin: Kai Tietz <KaiTietz.@onevision.com> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=gnu89" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

__attribute__((format(printf, 1, 2))) void foo (const char *, ...);

void bar (long long v1, long v2, int v3)
{
  foo ("%I64d %I32d %ld %d\n", v1, v2, v2, v3);
}

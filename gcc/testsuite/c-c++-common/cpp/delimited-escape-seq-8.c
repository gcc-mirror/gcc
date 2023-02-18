// CWG 2691 - hexadecimal-escape-sequence is too greedy
/* { dg-do run } */
/* { dg-require-effective-target wchar } */
/* { dg-options "-std=gnu99 -Wno-c++-compat" { target c } } */
/* { dg-options "-std=c++23" { target c++ } } */

#ifndef __cplusplus
#include <wchar.h>
typedef __CHAR16_TYPE__ char16_t;
typedef __CHAR32_TYPE__ char32_t;
#endif

const char32_t *a = U"\x{20}ab";

int
main ()
{
  if (a[0] != U'\x20' || a[1] != U'a' || a[2] != U'b' || a[3] != U'\0')
    __builtin_abort ();
}

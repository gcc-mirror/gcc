/* { dg-do compile } */
/* { dg-options "-std=c23 -fno-short-enums" } */

#include <limits.h>

enum E { a = 1ULL << (ULLONG_WIDTH - 5), b = 2 };
enum E { a = 1ULL << (ULLONG_WIDTH - 5), b = _Generic(a, enum E: 2) };


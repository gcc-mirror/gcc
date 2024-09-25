/* { dg-do compile } */
/* { dg-options "-std=c23 -fno-short-enums" } */

#include <limits.h>

enum E : int { a = 1, b = 2 };
enum E : int { b = _Generic(a, enum E: 2), a = 1 };

enum H { x = 1 };
enum H { x = 2ULL + UINT_MAX };	/* { dg-error "outside the range" } */

enum K : int { z = 1 };
enum K : int { z = 2ULL + UINT_MAX };	/* { dg-error "outside the range" } */

enum F { A = 0, B = UINT_MAX };
enum F { B = UINT_MAX, A };		/* { dg-error "outside the range" } */

enum G : unsigned int { C = 0, D = UINT_MAX };
enum G : unsigned int { D = UINT_MAX, C };		/* { dg-error "overflow" } */


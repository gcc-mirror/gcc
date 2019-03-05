/* Test AAPCS layout (alignment).  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_align-12.c"

struct s
{
  /* Should have 64-bit alignment.  */
  long long y : 57;
  char z: 7;
};

typedef struct s T;

#define EXPECTED_STRUCT_SIZE 8
extern void link_failure (void);
int
foo ()
{
  /* Optimization gets rid of this before linking.  */
  if (sizeof (struct s) != EXPECTED_STRUCT_SIZE)
    link_failure ();
}

T a = { 1, 4 };
T b = { 9, 16 };
T c = { 25, 36 };

#include "abitest.h"
#else
  ARG (int, 3, W0)
  ARG (T, a, X1)
  ARG (int, 5, W2)
  ARG (T, b, X3)
  ARG (__int128, 11, X4)
  ARG (__int128, 13, X6)
#ifndef __AAPCS64_BIG_ENDIAN__
  ARG (int, 7, STACK)
#else
  ARG (int, 7, STACK + 4)
#endif
  LAST_ARG (T, c, STACK + 8)
#endif

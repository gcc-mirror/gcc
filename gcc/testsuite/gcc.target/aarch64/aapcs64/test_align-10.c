/* Test AAPCS layout (alignment).  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_align-10.c"

struct s
{
  /* Should have 128-bit alignment.  */
  __int128 y : 65;
  char z: 7;
};

typedef struct s T;

#define EXPECTED_STRUCT_SIZE 16
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
  ARG (T, a, X2)
  ARG (int, 5, W4)
  ARG (T, b, X6)
#ifndef __AAPCS64_BIG_ENDIAN__
  ARG (int, 7, STACK)
#else
  ARG (int, 7, STACK + 4)
#endif
  /* Natural alignment should be 16.  */
  LAST_ARG (T, c, STACK + 16)
#endif

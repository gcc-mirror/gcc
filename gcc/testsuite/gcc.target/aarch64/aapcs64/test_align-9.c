/* Test AAPCS layout (alignment).  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_align-9.c"

struct s
  {
    /* This forces the alignment and size of the struct to 16.  */
    __attribute__ ((__aligned__ (16))) long x;
    int y;
    /* 4 bytes padding.  */
  };

typedef struct s __attribute__ ((__aligned__ (8))) underaligned;

#define EXPECTED_STRUCT_SIZE 16
extern void link_failure (void);
int
foo ()
{
  /* Optimization gets rid of this before linking.  */
  if (sizeof (struct s) != EXPECTED_STRUCT_SIZE)
    link_failure ();
}

underaligned a = { 1, 4 };
underaligned b = { 9, 16 };
underaligned c = { 25, 36 };

#include "abitest.h"
#else
  ARG (int, 3, W0)
  /* Object alignment is 16, so skip X1.  */
  ARG (underaligned, a, X2)
  ARG (int, 5, W4)
  /* Object alignment is 16, so skip X5.  */
  ARG (underaligned, b, X6)
#ifndef __AAPCS64_BIG_ENDIAN__
  ARG (int, 7, STACK)
#else
  ARG (int, 7, STACK + 4)
#endif
  /* Natural alignment should be 16.  */
  LAST_ARG (underaligned, c, STACK + 16)
#endif

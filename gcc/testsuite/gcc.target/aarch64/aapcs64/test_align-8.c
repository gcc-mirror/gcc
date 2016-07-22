/* Test AAPCS layout (alignment).  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_align-8.c"

/* The alignment also gives this size 32, so will be passed by reference.  */
typedef struct __attribute__ ((__aligned__ (32)))
  {
    long x;
    long y;
  } overaligned;

#define EXPECTED_STRUCT_SIZE 32
extern void link_failure (void);
int
foo ()
{
  /* Optimization gets rid of this before linking.  */
  if (sizeof (overaligned) != EXPECTED_STRUCT_SIZE)
    link_failure ();
}

overaligned a = { 2, 3 };

#include "abitest.h"
#else
  ARG (int, 7, W0)
  /* Alignment should be 8.  */
  PTR (overaligned, a, X1)
  LAST_ARG (int, 9, W2)
#endif

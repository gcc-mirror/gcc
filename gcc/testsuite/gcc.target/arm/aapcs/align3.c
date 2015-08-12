/* Test AAPCS layout (alignment).  */

/* { dg-do run { target arm_eabi } } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O3" } */

#ifndef IN_FRAMEWORK
#define TESTFILE "align3.c"

/* Struct will be aligned to 8.  */
struct s
  {
    int x;
    /* 4 bytes padding here.  */
    __attribute__((aligned (8))) int y;
    /* 4 bytes padding here.  */
  };

typedef struct s __attribute__((aligned (4))) underaligned;

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

#include "abitest.h"
#else
  ARG (int, 3, R0)
  /* Object alignment is 8, so split between 2 regs and 8 on stack.  */
  ARG (underaligned, a, R2)
  ARG (int, 6, STACK + 8)
  /* Object alignment is 8, so skip over STACK + 12.  */
  LAST_ARG (underaligned, b, STACK + 16)
#endif

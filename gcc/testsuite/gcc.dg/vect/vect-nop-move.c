/* { dg-do run } */ 
/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-fdump-rtl-combine-details" } */

#include "tree-vect.h"

extern void abort (void);

#define NOINLINE __attribute__((noinline))

typedef float float32x4_t __attribute__ ((__vector_size__ (16)));
typedef float float32x2_t __attribute__ ((__vector_size__ (8)));

NOINLINE float
foo32x4_be (float32x4_t x)
{
  return x[3];
}

NOINLINE float
foo32x4_le (float32x4_t x)
{
  return x[0];
}

NOINLINE float
bar (float a)
{
  return a;
}

NOINLINE float
foo32x2_be (float32x2_t x)
{
#ifdef __i386__
  /* ix86 passes float32x2 vector arguments in mmx registers.  We need to
     emit emms to empty MMS state and reenable x87 stack before float value
     can be loaded to and passed in x87 floating-point return register.  */
  __builtin_ia32_emms ();
#endif
  return bar (x[1]);
}

NOINLINE float
foo32x2_le (float32x2_t x)
{
#ifdef __i386__
  __builtin_ia32_emms ();
#endif
  return bar (x[0]);
}

NOINLINE int
test (void)
{
  float32x4_t a = { 0.0f, 1.0f, 2.0f, 3.0f };
  float32x2_t b = { 0.0f, 1.0f };

  if (foo32x2_be (b) != 1.0f)
    abort ();

  if (foo32x2_le (b) != 0.0f)
    abort ();

  if (foo32x4_be (a) != 3.0f)
    abort ();

  if (foo32x4_le (a) != 0.0f)
    abort ();

  return 0;
}

int
main ()
{
  check_vect ();
  return test ();
}

/* { dg-final { scan-rtl-dump "deleting noop move" "combine" { target aarch64*-*-* } } } */
/* { dg-final { cleanup-rtl-dump "combine" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

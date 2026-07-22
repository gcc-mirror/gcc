/* { dg-do compile } */
/* { dg-require-effective-target powerpc_future_compile_ok } */
/* { dg-options "-mdejagnu-cpu=future -mdense-math -O2" } */

/* The __dmr1024 opaque type, like __vector_quad and __vector_pair
   (see pr96506-2.c), may not be used as a function return value.
   Verify we flag errors on these uses rather than ICE.  Kept separate
   from the function-parameter test (dmr1024-invalid-use-1.c); see the
   comment there.  */

typedef __dmr1024 dmr_t;

__dmr1024
foo0 (__dmr1024 *src)
{ /* { dg-error "invalid use of Dense Math type .__dmr1024. as a function return value" } */
  return *src;
}

dmr_t
foo1 (dmr_t *src)
{ /* { dg-error "invalid use of Dense Math type .__dmr1024. as a function return value" } */
  return *src;
}

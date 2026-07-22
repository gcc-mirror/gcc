/* { dg-do compile } */
/* { dg-require-effective-target powerpc_future_compile_ok } */
/* { dg-options "-mdejagnu-cpu=future -mdense-math -O2 -std=gnu17" } */

/* The __dmr1024 opaque type, like __vector_quad and __vector_pair
   (see pr96506-1.c), may not be used as a function parameter.  Verify
   we flag errors on these uses rather than ICE.

   This must be kept separate from the return-value test
   (dmr1024-invalid-use-2.c): the parameter diagnostics below are only
   emitted during RTL expansion, which is skipped entirely once a
   parse-time error (such as the return-value diagnostic) has been
   emitted.  */

extern void bar0 ();
extern void bar1 ();

typedef __dmr1024 dmr_t;

void
foo0 (void)
{
  __dmr1024 v;
  bar0 (v); /* { dg-error "invalid use of Dense Math operand of type .__dmr1024. as a function parameter" } */
}

void
foo1 (void)
{
  dmr_t v;
  bar1 (v); /* { dg-error "invalid use of Dense Math operand of type .__dmr1024. as a function parameter" } */
}

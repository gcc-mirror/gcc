/* { dg-do compile } */
/* { dg-require-effective-target powerpc_future_compile_ok } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */

typedef unsigned char vec_t __attribute__((vector_size(16)));

void
shapad_1_0_0(__dmr1024 *res, vec_t pd) /* { dg-error "Invalid combination" } */
{
    __builtin_dmxxshapad(res, pd, 1, 0, 0);
}

void
shapad_1_0_1(__dmr1024 *res, vec_t pd) /* { dg-error "Invalid combination" } */
{
  __builtin_dmxxshapad(res, pd, 1, 0, 1);
}

void
shapad_1_1_0(__dmr1024 *res, vec_t pd) /* { dg-error "Invalid combination" } */
{
  __builtin_dmxxshapad(res, pd, 1, 1, 0);
}

void
shapad_1_1_1(__dmr1024 *res, vec_t pd) /* { dg-error "Invalid combination" } */
{
  __builtin_dmxxshapad(res, pd, 1, 1, 1);
}

// There are other error messages emitted in expand phase. Ignore them.
/* { dg-excess-errors "" } */

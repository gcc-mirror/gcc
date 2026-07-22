/* { dg-do compile } */
/* { dg-require-effective-target powerpc_future_compile_ok } */
/* { dg-options "-mdejagnu-cpu=future -mdense-math -mno-mma -O2" } */

/* Dense Math (-mdense-math) without MMA supports __vector_pair and
   __dmr1024 (see dmf-no-mma-1.c), but __vector_quad moves still
   require MMA.  Verify that a __vector_quad copy under -mno-mma is
   diagnosed with a proper error message via the movxo expander
   instead of ICEing.  The error is emitted during RTL expansion, so
   its reported location can be fragile; match it anywhere in the
   output.  */

void
copy_quad (__vector_quad *dst, __vector_quad *src)
{
  *dst = *src;
}

/* { dg-error "type '__vector_quad' requires the '-mmma' option" "" { target *-*-* } 0 } */

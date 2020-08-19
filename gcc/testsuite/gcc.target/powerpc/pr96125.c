/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power7 -O2" } */

void
__attribute__((target("cpu=power10")))
test0 (__vector_quad *dst)
{
  __vector_quad acc;
  __builtin_mma_xxsetaccz (&acc);
  *dst = acc;
}

void
test1 (__vector_quad *dst)
{
  __vector_quad acc;
  __builtin_mma_xxsetaccz (&acc); /* { dg-error "'__builtin_mma_xxsetaccz' requires the '-mmma' option" } */
  *dst = acc;
}

#pragma GCC target("cpu=power10")
void
test2 (__vector_quad *dst)
{
  __vector_quad acc;
  __builtin_mma_xxsetaccz (&acc);
  *dst = acc;
}

void
test3 (__vector_quad *dst)
{
  __vector_quad acc;
  __builtin_mma_xxsetaccz (&acc);
  *dst = acc;
}

#pragma GCC reset_options
void
test4 (__vector_quad *dst)
{
  __vector_quad acc;
  __builtin_mma_xxmfacc (&acc); /* { dg-error "'__builtin_mma_xxmfacc' requires the '-mmma' option" } */
  *dst = acc;
}


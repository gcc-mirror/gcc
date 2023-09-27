/* { dg-do compile } */
/* Verify that relaxed atomic stores use simple store instuctions.  */
/* { dg-final { scan-assembler-not {\mamoswap} } } */

void
foo(int bar, int baz)
{
  __atomic_store_n(&bar, baz, __ATOMIC_RELAXED);
}

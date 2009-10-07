/* Verify that only one DW_AT_const_value is emitted for baz,
   not for baz abstract DIE and again inside of
   DW_TAG_inlined_subroutine.  */
/* { dg-options "-O2 -g -dA" } */
/* { dg-do compile } */
/* { dg-final { scan-assembler-times " DW_AT_const_value" 1 } } */

struct A { const long i; const long j; };

static inline long
foo (void)
{
  const struct A baz = { .i = 2, .j = 21 };
  asm volatile ("" : : : "memory");
  return baz.i * baz.j;
}

int
main ()
{
  return foo () - 42;
}

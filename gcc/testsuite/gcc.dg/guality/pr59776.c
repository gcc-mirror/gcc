/* PR debug/59776 */
/* { dg-do run } */
/* { dg-options "-g" } */

#include "../nop.h"

struct S { float f, g; };

__attribute__((noinline, noclone)) void
foo (struct S *p)
{
  struct S s1, s2;			/* { dg-final { gdb-test pr59776.c:17 "s1.f" "5.0" } } */
  s1 = *p;				/* { dg-final { gdb-test pr59776.c:17 "s1.g" "6.0" } } */
  s2 = s1;				/* { dg-final { gdb-test pr59776.c:17 "s2.f" "0.0" } } */
  *(int *) &s2.f = 0;			/* { dg-final { gdb-test pr59776.c:17 "s2.g" "6.0" } } */
  asm volatile (NOP : : : "memory");	/* { dg-final { gdb-test pr59776.c:20 "s1.f" "5.0" } } */
  asm volatile (NOP : : : "memory");	/* { dg-final { gdb-test pr59776.c:20 "s1.g" "6.0" } } */
  s2 = s1;				/* { dg-final { gdb-test pr59776.c:20 "s2.f" "5.0" } } */
  asm volatile (NOP : : : "memory");	/* { dg-final { gdb-test pr59776.c:20 "s2.g" "6.0" } } */
  asm volatile (NOP : : : "memory");
}

int
main ()
{
  struct S x = { 5.0f, 6.0f };
  foo (&x);
  return 0;
}

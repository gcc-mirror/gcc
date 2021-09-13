/* PR debug/59776 */
/* { dg-do run } */
/* { dg-options "-g" } */

#include "../nop.h"

struct S { float f, g; };

__attribute__((noipa)) void
foo (struct S *p)
{
  struct S s1, s2;			/* { dg-final { gdb-test pr59776.c:17 "s1.f" "5.0" { xfail { aarch64*-*-* && { any-opts "-Og" } } } } } */
  s1 = *p;				/* { dg-final { gdb-test pr59776.c:17 "s1.g" "6.0" { xfail { aarch64*-*-* && { any-opts "-Og" } } } } } */
  s2 = s1;				/* { dg-final { gdb-test pr59776.c:17 "s2.f" "0.0" { xfail { aarch64*-*-* && { any-opts "-Og" } } } } } */
  *(int *) &s2.f = 0;			/* { dg-final { gdb-test pr59776.c:17 "s2.g" "6.0" { xfail { no-opts "-O0" } } } } */
  asm volatile (NOP : : : "memory");	/* { dg-final { gdb-test pr59776.c:20 "s1.f" "5.0" { xfail { aarch64*-*-* && { any-opts "-Og" } } } } } */
  asm volatile (NOP : : : "memory");	/* { dg-final { gdb-test pr59776.c:20 "s1.g" "6.0" { xfail { aarch64*-*-* && { any-opts "-Og" } } } } } */
  s2 = s1;				/* { dg-final { gdb-test pr59776.c:20 "s2.f" "5.0" { xfail { aarch64*-*-* && { any-opts "-Og" } } } } } */
  asm volatile (NOP : : : "memory");	/* { dg-final { gdb-test pr59776.c:20 "s2.g" "6.0" { xfail { no-opts "-O0" } } } } */
  asm volatile (NOP : : : "memory");
}

int
main ()
{
  struct S x = { 5.0f, 6.0f };
  foo (&x);
  return 0;
}

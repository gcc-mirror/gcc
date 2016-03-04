/* PR debug/69947 */
/* { dg-do run } */
/* { dg-options "-g" } */

#include "../nop.h"

static const char *c = "foobar";

__attribute__((noinline, noclone)) void
foo (void)
{
  static const char a[] = "abcdefg";
  const char *b = a;		/* { dg-final { gdb-test 14 "c\[2\]" "'o'" } } */
  asm (NOP : : : "memory");	/* { dg-final { gdb-test 14 "b\[4\]" "'e'" } } */
}

int
main ()
{
  foo ();
  return 0;
}

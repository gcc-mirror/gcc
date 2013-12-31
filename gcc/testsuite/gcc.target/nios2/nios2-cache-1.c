/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "ldwio" } } */
/* { dg-final { scan-assembler-not "stwio" } } */

/* Make sure the default behavior is not to generate I/O variants of
   the load and stores to foo.  */

extern volatile int foo;

int
read_foo (void)
{
  return foo;
}

void
write_foo (int x)
{
  foo = x;
}

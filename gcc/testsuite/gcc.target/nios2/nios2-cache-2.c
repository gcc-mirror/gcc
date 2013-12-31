/* { dg-do compile } */
/* { dg-options "-O2 -mno-cache-volatile" } */
/* { dg-final { scan-assembler "ldwio" } } */
/* { dg-final { scan-assembler "stwio" } } */

/* Make sure -mno-cache-volatile generates I/O variants of the load and
   stores to foo.  */

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

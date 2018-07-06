/* { dg-do compile } */
/* { dg-options "-O1" } */
struct S { int c, *b, *e; };

void
foo ()
{
  struct S a;
  asm volatile ("" : "=rm" (a) : "0" (1)); /* { dg-error "inconsistent operand constraints in an 'asm'" } */
}

/* PR c/28649 */
/* { dg-do compile } */

void foo()
{
  +;  /* { dg-error "expected expression" } */
}

int i;

void bar()
{
  i++;  /* { dg-bogus "undeclared" } */
}

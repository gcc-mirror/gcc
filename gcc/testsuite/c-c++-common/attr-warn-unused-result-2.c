/* PR c/82134 */
/* { dg-do compile } */
/* { dg-additional-options -Wno-c++-compat { target c } } */

struct S {};

__attribute__((warn_unused_result)) struct S foo();

void use_s(struct S);

void
test (void)
{
  struct S s = foo(); /* { dg-bogus "ignoring return value of" } */
  use_s(foo()); /* { dg-bogus "ignoring return value of" } */
}

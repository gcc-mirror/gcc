/* Test that #pragma GCC visibility does not override class member specific settings. */
/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-assembler "\\.internal.*Foo.methodEv" } } */

#pragma GCC visibility push(hidden)
class Foo
{
  __attribute__ ((visibility ("internal"))) void method();
};
#pragma GCC visibility pop

void Foo::method() { }

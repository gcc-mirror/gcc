/* Test that #pragma GCC visibility affects class members. */
/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-assembler "\\.hidden.*Foo.methodEv" } } */

#pragma GCC visibility push(hidden)
class Foo
{
  void method();
};
#pragma GCC visibility pop

void Foo::method() { }

/* Test that #pragma GCC visibility does not override class member specific settings. */
/* { dg-do compile } */
/* { dg-require-visibility "internal" } */
/* { dg-final { scan-assembler "\\.internal.*Foo.methodEv" { target { ! { *-*-solaris2* *-*-darwin* *-*-aix* } } } } } */
/* { dg-final { scan-assembler "\\.(internal|hidden).*Foo.methodEv" { target *-*-solaris2* } } } */

#pragma GCC visibility push(hidden)
class __attribute__ ((visibility ("internal"))) Foo
{
  void method();
};
#pragma GCC visibility pop

void Foo::method() { }

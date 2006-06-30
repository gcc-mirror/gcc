/* Test that -fvisibility does not override class member specific settings. */
/* { dg-do compile } */
/* { dg-require-visibility "internal" } */
/* { dg-options "-fvisibility=hidden" } */
/* { dg-final { scan-not-hidden "Foo.methodEv" } } */

class Foo
{
  __attribute__ ((visibility ("internal"))) void method();
};

void Foo::method() { }

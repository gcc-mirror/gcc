/* Test that -fvisibility does not override class member specific settings. */
/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-options "-fvisibility=hidden" } */
/* { dg-final { scan-not-hidden "methodEv" } } */

class __attribute__ ((visibility ("default"))) Foo
{
  void method();
};

void Foo::method() { }

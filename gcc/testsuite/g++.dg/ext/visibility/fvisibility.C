/* Test that -fvisibility affects class members. */
/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-options "-fvisibility=hidden" } */
/* { dg-final { scan-hidden "_ZN3Foo6methodEv" } } */

class Foo
{
  void method();
};

void Foo::method() { }

/* Test that setting visibility for static class member functions works. */
/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-hidden "_ZN3Foo6methodEv" } } */

class __attribute__ ((visibility ("hidden"))) Foo
{
  static void method();
};

void Foo::method() { }

/* Test that setting visibility for class member functions works. */
/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-assembler "\\.hidden.*Foo.methodEv" } } */

class __attribute__ ((visibility ("hidden"))) Foo
{
  void method();
};

void Foo::method() { }

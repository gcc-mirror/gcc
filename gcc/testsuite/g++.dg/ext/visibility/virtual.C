/* Test that setting visibility for class affects virtual table. */
/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-hidden "ZTV3Foo" } } */

class __attribute__ ((visibility ("hidden"))) Foo
{
  virtual void method();
};

void Foo::method() { }

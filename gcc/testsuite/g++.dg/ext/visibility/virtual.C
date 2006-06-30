/* Test that setting visibility for class affects virtual table, VTT and
   type_info name and node. */
/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-hidden "ZTV3Foo" } } */
/* { dg-final { scan-hidden "ZTT3Foo" } } */
/* { dg-final { scan-hidden "ZTS3Foo" } } */
/* { dg-final { scan-hidden "ZTI3Foo" } } */

struct A { };

class __attribute__ ((visibility ("hidden"))) Foo: virtual public A
{
  virtual void method();
};

void Foo::method() { }

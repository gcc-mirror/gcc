// PR c++/64266
/* { dg-do compile } */

extern "C" int printf (const char *, ...);

struct A
{
  void foo(int i)
  {
    printf ("__FUNCTION__ = %s\n", __FUNCTION__);
    printf ("__PRETTY_FUNCTION__ = %s\n", __PRETTY_FUNCTION__);
  }

  void foo()
  {
     printf ("__FUNCTION__ = %s\n", __FUNCTION__);
  }
};

int
main ()
{
  A a;
  a.foo (0);
  a.foo ();
  return 0;
}

/* { dg-final { scan-assembler-not "_ZZN1A3fooEvE12__FUNCTION__" } } */
/* { dg-final { scan-assembler-not "_ZZN1A3fooEiE12__FUNCTION__" } } */
/* { dg-final { scan-assembler-not "_ZZN1A3fooEiE19__PRETTY_FUNCTION__" } } */
/* { dg-final { scan-assembler ".(string|ascii) \"void A::foo\\(int\\)(.0)?\"" } } */
/* { dg-final { scan-assembler ".(string|ascii) \"foo(.0)?\"" } } */

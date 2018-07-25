/* { dg-lto-do link } */
/* { dg-require-effective-target fpic } */
/* { dg-lto-options {{-flto -flto-partition=1to1 -fPIC -r -nostdlib}} } */
/* { dg-extra-ld-options "-flinker-output=nolto-rel" } */

/* Tests for the absence during linking of:
   lto1: error: type of '_ZTVN10__cxxabiv120__si_class_type_infoE' does
         not match original declaration  */

struct Foo { virtual ~Foo(); };
namespace __cxxabiv1
{
  struct __si_class_type_info: public Foo { };
  struct Baz: public Foo { virtual void Func(); };
  void Baz::Func() { }
}

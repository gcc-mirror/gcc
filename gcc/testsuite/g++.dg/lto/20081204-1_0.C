/* { dg-lto-do link } */
/* { dg-lto-options {{-fwhopr -fPIC -r -nostdlib}} } */

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

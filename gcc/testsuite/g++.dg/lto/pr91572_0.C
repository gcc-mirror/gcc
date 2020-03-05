// PR lto/91572
// { dg-lto-do link }
// { dg-lto-options { { -O -fPIC -flto } } }
// { dg-require-effective-target shared }
// { dg-require-effective-target fpic }
// { dg-extra-ld-options "-shared" }

void foo (char);
namespace N {
  class A { A (); };
  A::A () { asm ("" : : "g" (0)); }
}

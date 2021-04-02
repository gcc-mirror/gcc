// PR c++/94775
// { dg-do compile { target c++11 } }
// { dg-additional-options "-mstrict-align" { target { aarch64*-*-* powerpc*-*-linux* powerpc*-*-elf* } } }

struct alignas(8) S {
  S *arr[1];
  void fn () const { (void) arr[0]; }
};

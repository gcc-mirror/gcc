/* { dg-lto-do link } */
/* { dg-lto-options "-O2  -Wno-odr" } */
/* { dg-extra-ld-options { -O2 -Wno-odr -r -nostdlib } } */
namespace std {
class ios_base {
  struct A {};
  class __attribute((__abi_tag__("cxx11"))) failure : A {};
} a;
}

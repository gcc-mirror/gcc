/* { dg-lto-do link } */
/* { dg-lto-options "-O2  -Wno-odr" } */
/* { dg-extra-ld-options { -O2 -Wno-odr -r -nostdlib } } */
namespace std {
class exception {};
class runtime_error : exception {
  virtual char m_fn1();
} a;
}

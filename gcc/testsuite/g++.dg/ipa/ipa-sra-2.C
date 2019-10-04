/* { dg-do compile { target c++11 } } */
/* { dg-options "-O2 -fipa-sra"  } */

class a {
  void b();
  char16_t c;
  char16_t d;
};
void e(a);
void g();
void a::b() {
  char16_t f = d;
  e(*this);
  for (;;) {
    g();
    if (f)
      break;
  }
}

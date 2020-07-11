// PR target/95229
// { dg-do compile }
// { dg-options "-O3 -march=znver1" }

struct a {
  unsigned long long b;
  unsigned long long c;
};

class my_class {
public:
  a d;
} e;

struct f {
  unsigned g;
  unsigned h;
  void i();
};

void f::i() {
  e.d.b += g;
  e.d.c += h;
}

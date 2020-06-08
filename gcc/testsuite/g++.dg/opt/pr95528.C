// PR target/95528
// { dg-do compile { target c++11 } }
// { dg-options "-O3" }
// { dg-additional-options "-march=skylake-avx512" { target i?86-*-*- x86_64-*-* } }

template <typename a> struct b {
  typedef a c __attribute__((vector_size(sizeof(a) * 4)));
  union {
    c d;
    struct {
      a e, f, g, h;
    };
  };
  b();
  b(const b &i) : d(i.d) {}
  static b j(c);
  template <typename k> operator b<k>() {
    b<k>::j(typename b<k>::c{k(e), k(f), k(g), k(h)});
    return b<k>();
  }
};
template <typename a> using l = b<a>;
using m = l<char>;
using n = l<short>;
m o(n i) { return i; }
b<short> q;
void p() { o(q); }

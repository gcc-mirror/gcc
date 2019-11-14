// Copied from pr87967.C
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -ftree-vectorize -fno-tree-pre --param vect-epilogues-nomask=1" }

void h();
template <typename b> struct k { using d = b; };
template <typename b, template <typename> class> using e = k<b>;
template <typename b, template <typename> class f>
using g = typename e<b, f>::d;
struct l {
  template <typename i> using ab = typename i::j;
};
struct n : l {
  using j = g<char *, ab>;
};
class o {
public:
  long r();
};
char m;
char s() {
  if (m)
    return '0';
  return 'A';
}
class t {
public:
  typedef char *ad;
  ad m_fn2();
};
void fn3() {
  char *a;
  t b;
  bool p = false;
  while (*a) {
    h();
    o c;
    if (*a)
      a++;
    if (c.r()) {
      n::j q;
      for (t::ad d = b.m_fn2(), e; d != e; d++) {
        char f = *q;
        *d = f + s();
      }
      p = true;
    }
  }
  if (p)
    throw;
}

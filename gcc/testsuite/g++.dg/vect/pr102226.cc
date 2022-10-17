// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-additional-options "-msve-vector-bits=128" { target aarch64_sve } }

template <typename a> struct b { using c = a; };
template <typename a, template <typename> class> using f = b<a>;
template <typename a, template <typename> class g>
using h = typename f<a, g>::c;
struct i {
  template <typename j> using k = typename j::l;
};
struct m : i {
  using l = h<char *, k>;
};
class n {
public:
  char operator[](long o) {
    m::l s;
    return s[o];
  }
} p;
n r;
int q() {
  long d;
  for (long e; e; e++)
    if (p[e] == r[e])
      d++;
  return d;
}

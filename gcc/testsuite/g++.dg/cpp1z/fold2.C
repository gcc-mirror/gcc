// { dg-do compile }
// { dg-options "-std=c++1z" }

// Check that we can fold over all of the operators required
// by the standard in every possible way.

#define COMMA ,

#define MAKE_FNS(name, op) \
  template<typename... Ts> \
    auto unary_left_ ## name (Ts... ts) { return (... op ts); } \
  template<typename... Ts> \
    auto unary_right_ ## name (Ts... ts) { return (ts op ...); } \
  template<typename T, typename... Ts> \
    auto binary_left_ ## name (T x, Ts... ts) { return (x op ... op ts); } \
  template<typename T, typename... Ts> \
    auto binary_right_ ## name (T x, Ts... ts) { return (ts op ... op x); }

// TODO: These are compile-only tests...
#define CHECK_FN(name) \
  unary_left_ ## name (a); \
  unary_left_ ## name (a, b, c); \
  unary_right_ ## name (a); \
  unary_right_ ## name (a, b, c); \
  binary_left_ ## name (a); \
  binary_left_ ## name (a, b, c, d); \
  binary_right_ ## name (d); \
  binary_right_ ## name (d, a, b, c);

MAKE_FNS (add, +);
MAKE_FNS (sub, -);
MAKE_FNS (mul, *);
MAKE_FNS (div, /);
MAKE_FNS (mod, %);
MAKE_FNS (bxor, ^);
MAKE_FNS (bor, |);
MAKE_FNS (band, &);
MAKE_FNS (lsh, <<);
MAKE_FNS (rsh, >>);

MAKE_FNS (assign, =);
MAKE_FNS (addi, +=);
MAKE_FNS (subi, -=);
MAKE_FNS (muli, *=);
MAKE_FNS (divi, /=);
MAKE_FNS (modi, %=);
MAKE_FNS (bxori, ^=);
MAKE_FNS (bori, |=);
MAKE_FNS (bandi, &=);
MAKE_FNS (lshi, <<=);
MAKE_FNS (rshi, >>=);

MAKE_FNS (eq, ==);
MAKE_FNS (ne, !=);
MAKE_FNS (lt, <);
MAKE_FNS (gt, >);
MAKE_FNS (le, <=);
MAKE_FNS (ge, >=);

MAKE_FNS (land, &&);
MAKE_FNS (lor, ||);

MAKE_FNS (comma, COMMA);
MAKE_FNS (dot_star, .*);
MAKE_FNS (arrow_star, ->*);

int main() {
  int a = 0, b = 0, c = 0, d = 0;

  CHECK_FN (add);
  CHECK_FN (sub);
  CHECK_FN (mul);
  CHECK_FN (div);
  CHECK_FN (mod);
  CHECK_FN (bxor);
  CHECK_FN (bor);
  CHECK_FN (band);
  CHECK_FN (lsh);
  CHECK_FN (rsh);

  // CHECK_FN (assign);
  CHECK_FN (addi);
  CHECK_FN (subi);
  CHECK_FN (muli);
  CHECK_FN (divi);
  CHECK_FN (modi);
  CHECK_FN (bxori);
  CHECK_FN (bori);
  CHECK_FN (bandi);
  CHECK_FN (lshi);
  CHECK_FN (rshi);

  CHECK_FN (eq);
  CHECK_FN (ne);
  CHECK_FN (lt);
  CHECK_FN (gt);
  CHECK_FN (le);
  CHECK_FN (ge);
  CHECK_FN (eq);
  CHECK_FN (ne);

  CHECK_FN (comma);

  struct X {
    int a;
  } x, *px = &x;

  int X::* pm = &X::a;
  unary_left_arrow_star (px, pm); // px ->* pm
  unary_right_arrow_star (px, pm); // px ->* pm
  binary_left_arrow_star (px, pm); // px ->* pm
  binary_right_arrow_star (pm, px); // px ->* pm

  unary_left_dot_star (x, pm); // x ->* pm
  unary_right_dot_star (x, pm); // x ->* pm
  binary_left_dot_star (x, pm); // x ->* pm
  binary_right_dot_star (pm, x); // x ->* pm
}

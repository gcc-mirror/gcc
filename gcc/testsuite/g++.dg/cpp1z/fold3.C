// { dg-do compile { target c++17 } }
// { dg-options "" }

// Check that empty expansions and required failures.

#define COMMA ,

#define MAKE_FN(name, op) \
  template<typename... Ts> \
    constexpr auto name (Ts... ts) { return (... op ts); } // { dg-error "empty" }

MAKE_FN (add, +);
MAKE_FN (sub, -);
MAKE_FN (mul, *);
MAKE_FN (div, /);
MAKE_FN (mod, %);
MAKE_FN (bxor, ^);
MAKE_FN (bor, |);
MAKE_FN (band, &);
MAKE_FN (lsh, <<);
MAKE_FN (rsh, >>);

MAKE_FN (assign, =);
MAKE_FN (addi, +=);
MAKE_FN (subi, -=);
MAKE_FN (muli, *=);
MAKE_FN (divi, /=);
MAKE_FN (modi, %=);
MAKE_FN (bxori, ^=);
MAKE_FN (bori, |=);
MAKE_FN (bandi, &=);
MAKE_FN (lshi, <<=);
MAKE_FN (rshi, >>=);

MAKE_FN (eq, ==);
MAKE_FN (ne, !=);
MAKE_FN (lt, <);
MAKE_FN (gt, >);
MAKE_FN (le, <);
MAKE_FN (ge, >);

MAKE_FN (land, &&);
MAKE_FN (lor, ||);

MAKE_FN (comma, COMMA);
MAKE_FN (dot_star, .*);
MAKE_FN (arrow_star, ->*);

int main() {
  static_assert(land() == true, "");
  static_assert(lor() == false, "");
  comma(); // No value to theck

  // These are all errors, but the error is emitted at the point
  // of instantiation (line 10).
  add();			// { dg-message "required from here" }
  mul();			// { dg-message "required from here" }
  bor();			// { dg-message "required from here" }
  band();			// { dg-message "required from here" }
  sub();			// { dg-message "required from here" }
  div();			// { dg-message "required from here" }
  mod();			// { dg-message "required from here" }
  lsh();			// { dg-message "required from here" }
  rsh();			// { dg-message "required from here" }
  assign();			// { dg-message "required from here" }
  addi();			// { dg-message "required from here" }
  subi();			// { dg-message "required from here" }
  muli();			// { dg-message "required from here" }
  divi();			// { dg-message "required from here" }
  modi();			// { dg-message "required from here" }
  bxor();			// { dg-message "required from here" }
  bxori();			// { dg-message "required from here" }
  bori();			// { dg-message "required from here" }
  bandi();			// { dg-message "required from here" }
  lshi();			// { dg-message "required from here" }
  rshi();			// { dg-message "required from here" }
  eq();				// { dg-message "required from here" }
  ne();				// { dg-message "required from here" }
  lt();				// { dg-message "required from here" }
  gt();				// { dg-message "required from here" }
  le();				// { dg-message "required from here" }
  ge();				// { dg-message "required from here" }
  dot_star();			// { dg-message "required from here" }
  arrow_star();			// { dg-message "required from here" }
}

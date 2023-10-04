// { dg-do compile { target c++17 } }
// { dg-options "" }

// Check that empty expansions and required failures.

#define COMMA ,

#define MAKE_FN(name, op) \
  template<typename... Ts> \
    constexpr auto name (Ts... ts) { return (... op ts); } // { dg-message "" }

MAKE_FN (add, +);		// { dg-message "" }
MAKE_FN (sub, -);		// { dg-message "" }
MAKE_FN (mul, *);		// { dg-message "" }
MAKE_FN (div, /);		// { dg-message "" }
MAKE_FN (mod, %);		// { dg-message "" }
MAKE_FN (bxor, ^);		// { dg-message "" }
MAKE_FN (bor, |);		// { dg-message "" }
MAKE_FN (band, &);		// { dg-message "" }
MAKE_FN (lsh, <<);		// { dg-message "" }
MAKE_FN (rsh, >>);		// { dg-message "" }

MAKE_FN (assign, =);		// { dg-message "" }
MAKE_FN (addi, +=);		// { dg-message "" }
MAKE_FN (subi, -=);		// { dg-message "" }
MAKE_FN (muli, *=);		// { dg-message "" }
MAKE_FN (divi, /=);		// { dg-message "" }
MAKE_FN (modi, %=);		// { dg-message "" }
MAKE_FN (bxori, ^=);		// { dg-message "" }
MAKE_FN (bori, |=);		// { dg-message "" }
MAKE_FN (bandi, &=);		// { dg-message "" }
MAKE_FN (lshi, <<=);		// { dg-message "" }
MAKE_FN (rshi, >>=);		// { dg-message "" }

MAKE_FN (eq, ==);		// { dg-message "" }
MAKE_FN (ne, !=);		// { dg-message "" }
MAKE_FN (lt, <);		// { dg-message "" }
MAKE_FN (gt, >);		// { dg-message "" }
MAKE_FN (le, <);		// { dg-message "" }
MAKE_FN (ge, >);		// { dg-message "" }

MAKE_FN (land, &&);
MAKE_FN (lor, ||);

MAKE_FN (comma, COMMA);
MAKE_FN (dot_star, .*);		// { dg-message "" }
MAKE_FN (arrow_star, ->*);	// { dg-message "" }

int main() {
  static_assert(land() == true, "");
  static_assert(lor() == false, "");
  comma(); // No value to theck

  // These are all errors, but the error is emitted at the point
  // of macro definition or expansion above.
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

// PR c++/89513
// { dg-do compile { target c++11 } }
// { dg-options "" }

constexpr bool foo ()
try {			// { dg-warning "function-try-block body of 'constexpr' function only available with" "" { target c++17_down } }
  return true;
} catch (...) {
  return false;
}			// { dg-error "body of 'constexpr' function" "" { target c++11_only } }

constexpr bool bar ()
try {			// { dg-warning "function-try-block body of 'constexpr' function only available with" "" { target c++17_down } }
  try {			// { dg-warning "'try' in 'constexpr' function only available with" "" { target c++17_down } }
    return true;
  } catch (int) {
    return false;
  }
} catch (...) {
  return false;
}			// { dg-error "not a return-statement" "" { target c++11_only } }

constexpr bool baz ()
{
  try { return true; } catch (...) { return false; }	// { dg-warning "'try' in 'constexpr' function only available with" "" { target c++17_down } }
}			// { dg-error "not a return-statement" "" { target c++11_only } }

struct S {
  constexpr S () try : m (1)	// { dg-warning "function-try-block body of 'constexpr' constructor only available with" "" { target c++17_down } }
  {
    try {		// { dg-warning "'try' in 'constexpr' function only available with" "" { target c++17_down } }
    } catch (int) {
    }
  } catch (...) {	// { dg-error "'constexpr' constructor does not have empty body" "" { target c++11_only } }
  }
  int m;
};

struct T {
  constexpr T ()
  try {			// { dg-warning "function-try-block body of 'constexpr' constructor only available with" "" { target c++17_down } }
  } catch (...) {
  }
};

// PR c++/89513
// { dg-do compile { target c++11 } }

constexpr bool foo ()
try {			// { dg-error "function-try-block body of 'constexpr' function only available with" "" { target c++17_down } }
  return true;
} catch (...) {		// { dg-error "compound-statement in 'constexpr' function" "" { target c++11_only } }
  return false;
}			// { dg-error "body of 'constexpr' function" "" { target c++11_only } }

constexpr bool bar ()
try {			// { dg-error "function-try-block body of 'constexpr' function only available with" "" { target c++17_down } }
  try {			// { dg-error "'try' in 'constexpr' function only available with" "" { target c++17_down } }
    return true;	// { dg-error "compound-statement in 'constexpr' function" "" { target c++11_only } .-1 }
  } catch (int) {	// { dg-error "compound-statement in 'constexpr' function" "" { target c++11_only } }
    return false;
  }
} catch (...) {		// { dg-error "compound-statement in 'constexpr' function" "" { target c++11_only } }
  return false;
}			// { dg-error "not a return-statement" "" { target c++11_only } }

constexpr bool baz ()
{
  try { return true; } catch (...) { return false; }	// { dg-error "'try' in 'constexpr' function only available with" "" { target c++17_down } }
}			// { dg-error "not a return-statement" "" { target c++11_only } }
			// { dg-error "compound-statement in 'constexpr' function" "" { target c++11_only } .-2 }

struct S {
  constexpr S () try : m (1)	// { dg-error "function-try-block body of 'constexpr' constructor only available with" "" { target c++17_down } }
  {
    try {		// { dg-error "'try' in 'constexpr' function only available with" "" { target c++17_down } }
    } catch (int) {	// { dg-error "compound-statement in 'constexpr' function" "" { target c++11_only } }
    }			// { dg-error "compound-statement in 'constexpr' function" "" { target c++11_only } .-2 }
  } catch (...) {	// { dg-error "'constexpr' constructor does not have empty body" "" { target c++11_only } }
  }
  int m;
};

struct T {
  constexpr T ()
  try {			// { dg-error "function-try-block body of 'constexpr' constructor only available with" "" { target c++17_down } }
  } catch (...) {	// { dg-error "compound-statement in 'constexpr' function" "" { target c++11_only } }
  }
};

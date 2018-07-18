// Test for proper error message formatting; the throw() should go inside
// the parens, as below.

void (*g() throw())();		// { dg-message "g\\(\\) throw" "" { target { ! c++17 } } }
				// { dg-message "g\\(\\) noexcept" "" { target c++17 } .-1 }
void (*g())();			// { dg-error "" }

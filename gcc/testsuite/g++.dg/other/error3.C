// Test for proper error message formatting; the throw() should go inside
// the parens, as below.

void (*g() throw())();		// { dg-message "g\\(\\) throw" "" { target { ! c++1z } } }
				// { dg-message "g\\(\\) noexcept" "" { target c++1z } .-1 }
void (*g())();			// { dg-error "" "" }

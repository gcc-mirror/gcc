// Test for proper error message formatting; the throw() should go inside
// the parens, as below.

void (*g() throw())();		// { dg-message "g\\(\\) throw" "" }
void (*g())();			// { dg-error "" "" }

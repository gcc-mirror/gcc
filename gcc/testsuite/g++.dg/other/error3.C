// Test for proper error message formatting; the throw() should go inside
// the parens, as below.

void (*g() throw())();		// { dg-error "g\\(\\) throw" "" }
void (*g())();			// { dg-error "" "" }

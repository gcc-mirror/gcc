// Build don't link:
// Special g++ Options:

struct X {};
X x = X();		// gets bogus error

// Build don't link:

struct foo {
	template<typename T> static void bar( foo* );
	template<typename T> void bar() const; // gets bogus error - quals XFAIL *-*-*
};

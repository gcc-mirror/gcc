// { dg-do assemble  }

struct foo {
	template<typename T> static void bar( foo* );
	template<typename T> void bar() const; // { dg-bogus "" } quals
};

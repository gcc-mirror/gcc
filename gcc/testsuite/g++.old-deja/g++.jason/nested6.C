// Build don't link:
union A {
 struct B { };
 A::B b;			// gets bogus error
};

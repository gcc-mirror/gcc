// Build don't link:

template <class T> void foo();	// ERROR - candidate

void (*bar)() = foo<void>;
void (*baz)() = foo; // ERROR - can't deduce T

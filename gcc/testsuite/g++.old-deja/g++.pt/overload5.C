// Build don't link:

// crash test - XFAIL *-*-*

template <class T> void foo();

void (*bar)() = foo<void>;
void (*baz)() = foo; // ERROR - can't deduce T

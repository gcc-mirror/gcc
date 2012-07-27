// { dg-do assemble  }

template <class T> void foo();	// { dg-message "" } candidate

void (*bar)() = foo<void>;
void (*baz)() = foo; // { dg-error "" } can't deduce T

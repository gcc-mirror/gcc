// { dg-do assemble  }

template <class T> void foo();	// { dg-error "" } candidate

void (*bar)() = foo<void>;
void (*baz)() = foo; // { dg-error "" } can't deduce T

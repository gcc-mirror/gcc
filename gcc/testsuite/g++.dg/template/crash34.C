// { dg-do compile }

// PR c++/20028

// We used to crash when referencing TYPE_SIZE_UNIT of the messed-up
// type used for x, because it was not initialized.

class Foo;

template <typename T> class Foo { }; // { dg-error "not a template" }

Foo<int> x; // { dg-error "not a template|incomplete type" }

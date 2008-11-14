// Make sure that digest_init converts to the declared type of the
// bitfield, not just the lowered type.

enum E { EA, EB };

struct A { E e: 8; };

A a = { 0 };			// { dg-error "invalid conversion" }

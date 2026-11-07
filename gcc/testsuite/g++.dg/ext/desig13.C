// PR c++/127516
// This shouldn't ICE.

typedef int __attribute__((vector_size(8))) V;

V v = { .v = 42 };		// { dg-error "designated" }

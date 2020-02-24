// PR c++/93710 - poor diagnostic for array initializer.

struct A { A (int); A (char*); int i; };

int x;

A a1[] = {
  0L, // { dg-error "3:conversion from .long int. to .A. is ambiguous" }
  &x, // { dg-error "3:invalid conversion from .int\\*. to .int." }
  __builtin_offsetof (A, i) // { dg-error "23:conversion from .\(long \)?unsigned int. to .A. is ambiguous" }
};

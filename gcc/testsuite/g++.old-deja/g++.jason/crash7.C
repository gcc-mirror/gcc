// { dg-do assemble  }
// Bug: g++ can't deal.

typedef unsigned size_t;	// { dg-error "" } previous declaration
typedef unsigned long size_t;	// { dg-error "" } redefining size_t
void f (size_t);		// causes compiler segfault - 

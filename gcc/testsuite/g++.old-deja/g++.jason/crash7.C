// Bug: g++ can't deal.

typedef unsigned size_t;	// ERROR - previous declaration
typedef unsigned long size_t;	// ERROR - redefining size_t
void f (size_t);		// causes compiler segfault - 

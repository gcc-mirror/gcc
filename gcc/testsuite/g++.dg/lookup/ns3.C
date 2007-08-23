// PR c++/32898

namespace N { }

int N::i;	// { dg-error "should have been declared inside" }

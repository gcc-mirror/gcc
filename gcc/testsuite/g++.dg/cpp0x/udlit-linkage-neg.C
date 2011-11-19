// { dg-options -std=c++0x }

extern "C"_badlinkage {	// { dg-error "expected unqualified-id before" }

int foo();

}

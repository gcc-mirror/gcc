// { dg-options -std=c++0x }

extern "C" {

int
operator"" _badclinkage(unsigned long long);	// { dg-error "operator with C linkage" }

}

// { dg-do compile { target c++11 } }

extern "C" {

int
operator"" _badclinkage(unsigned long long);	// { dg-error "operator with C linkage" }

}

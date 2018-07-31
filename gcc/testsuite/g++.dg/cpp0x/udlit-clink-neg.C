// { dg-do compile { target c++11 } }

extern "C" {

int
operator"" _badclinkage(unsigned long long);	// { dg-error "1:literal operator with C linkage" }

}

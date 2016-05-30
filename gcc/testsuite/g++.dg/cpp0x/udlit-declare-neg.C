// { dg-do compile { target c++11 } }

//  Check that undeclared literal operator calls and literals give appropriate errors.

int i = operator"" _Bar('x');  // { dg-error "9:'operator\"\"_Bar' was not declared in this scope" }
int j = 'x'_Bar;  // { dg-error "unable to find character literal operator|with|argument" }

int ii = operator"" _BarCharStr("Howdy, Pardner!");  // { dg-error "10:'operator\"\"_BarCharStr' was not declared in this scope" }
int jj = "Howdy, Pardner!"_BarCharStr;  // { dg-error "unable to find string literal operator|Possible missing length argument" }

unsigned long long iULL = operator"" _BarULL(666ULL);  // { dg-error "27:'operator\"\"_BarULL' was not declared in this scope" }
unsigned long long jULL = 666_BarULL;  // { dg-error "unable to find numeric literal operator" }

long double iLD = operator"" _BarLD(666.0L);  // { dg-error "19:'operator\"\"_BarLD' was not declared in this scope" }
long double jLD = 666.0_BarLD;  // { dg-error "unable to find numeric literal operator" }

// { dg-options -std=c++0x }

char32_t
operator"" (char32_t C)	// { dg-error "expected suffix identifier" }
{ return C; }

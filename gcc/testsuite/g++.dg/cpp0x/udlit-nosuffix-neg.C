// { dg-do compile { target c++11 } }

char32_t
operator ""(char32_t C)	// { dg-error "expected suffix identifier" }
{ return C; }

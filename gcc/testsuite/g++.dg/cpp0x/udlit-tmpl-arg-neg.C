// { dg-options -std=c++0x }

template<char...>
  int operator"" _xyz(unsigned long long);	// { dg-error "has invalid argument list" }

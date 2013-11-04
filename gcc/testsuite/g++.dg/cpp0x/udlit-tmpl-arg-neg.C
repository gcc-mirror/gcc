// { dg-options -std=c++11 }

template<char...>
  int operator"" _xyz(unsigned long long);	// { dg-error "has invalid argument list" }

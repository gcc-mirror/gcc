// { dg-do compile { target c++11 } }

template<char...>
  int operator"" _xyz(unsigned long long);	// { dg-error "has invalid argument list" }

// { dg-options "-std=c++11" }

//  Can't have *both* literal operator template and raw literal operator.

int
operator"" _abc(const char*)
  {
    return 42;
  }

template<char...>
  int
  operator"" _abc() // { dg-error "literal operator template|conflicts with raw literal operator" }
  {
    return 13;
  }

template<char...>
  int
  operator"" _def()
  {
    return 12;
  }

int
operator"" _def(const char*) // { dg-error "raw literal operator|conflicts with literal operator template" }
  {
    return 43;
  }

int
operator"" _ghi(long double)
  {
    return 42;
  }

template<char...>
  int
  operator"" _ghi() // OK
  {
    return 13;
  }

// P0847R7
// { dg-do compile { target c++23 } }

// instantiating captureless lambda call operator with unrelated explicit object parameter

void test0()
{
  auto f0 = [](this auto self) { return self; };
  auto fp0_value     = static_cast<int(*)(int)        >(&decltype(f0)::operator());
  auto fp0_lref      = static_cast<int(*)(int&)       >(&decltype(f0)::operator());
  auto fp0_rref      = static_cast<int(*)(int&&)      >(&decltype(f0)::operator());
  auto fp0_constlref = static_cast<int(*)(int const&) >(&decltype(f0)::operator());
  auto fp0_constrref = static_cast<int(*)(int const&&)>(&decltype(f0)::operator());

  auto f1 = [](this auto&& self) { return self; };
  auto fp1_value      = static_cast<int(*)(int)        >(&decltype(f1)::operator()); // { dg-error {invalid 'static_cast' from type} }
  auto fp1_lref       = static_cast<int(*)(int&)       >(&decltype(f1)::operator());
  auto fp1_rref       = static_cast<int(*)(int&&)      >(&decltype(f1)::operator());
  auto fp1_constlref  = static_cast<int(*)(int const&) >(&decltype(f1)::operator());
  auto fp1_constrref  = static_cast<int(*)(int const&&)>(&decltype(f1)::operator());
}

void test1()
{
  auto f0 = [](this auto self) { return self; };
  int (*fp0_value)(int)             = &decltype(f0)::operator();
  int (*fp0_lref)(int&)             = &decltype(f0)::operator();
  int (*fp0_rref)(int&&)            = &decltype(f0)::operator();
  int (*fp0_constlref)(int const&)  = &decltype(f0)::operator();
  int (*fp0_constrref)(int const&&) = &decltype(f0)::operator();

  auto f1 = [](this auto&& self) { return self; };
  int (*fp1_value)(int)              = &decltype(f1)::operator(); // { dg-error {no matches converting function} }
  int (*fp1_lref)(int&)              = &decltype(f1)::operator();
  int (*fp1_rref)(int&&)             = &decltype(f1)::operator();
  int (*fp1_constlref)(int const&)   = &decltype(f1)::operator();
  int (*fp1_constrref)(int const&&)  = &decltype(f1)::operator();
}


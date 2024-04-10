// P0847R7
// { dg-do run { target c++23 } }

// calling captureless lambda call operator with unrelated explicit object parameter
// through function pointer

int main()
{
  auto f0 = [](this auto self) { return self; };
  auto fp0_value     = static_cast<int(*)(int)        >(&decltype(f0)::operator());
  auto fp0_lref      = static_cast<int(*)(int&)       >(&decltype(f0)::operator());
  auto fp0_rref      = static_cast<int(*)(int&&)      >(&decltype(f0)::operator());
  auto fp0_constlref = static_cast<int(*)(int const&) >(&decltype(f0)::operator());
  auto fp0_constrref = static_cast<int(*)(int const&&)>(&decltype(f0)::operator());

  auto f1 = [](this auto&& self) { return self; };
  auto fp1_lref      = static_cast<int(*)(int&)       >(&decltype(f1)::operator());
  auto fp1_rref      = static_cast<int(*)(int&&)      >(&decltype(f1)::operator());
  auto fp1_constlref = static_cast<int(*)(int const&) >(&decltype(f1)::operator());
  auto fp1_constrref = static_cast<int(*)(int const&&)>(&decltype(f1)::operator());

  // both are needed for lvalue/rvalue overloads
  #define MAGIC 42
  int magic = MAGIC;

  if (fp0_value (magic) != magic)
    __builtin_abort ();
  if (fp0_lref (magic) != magic)
    __builtin_abort ();
  if (fp0_rref (MAGIC) != magic)
    __builtin_abort ();
  if (fp0_constlref (magic) != magic)
    __builtin_abort ();
  if (fp0_constrref (MAGIC) != magic)
    __builtin_abort ();

  if (fp1_lref (magic) != magic)
    __builtin_abort ();
  if (fp1_rref (MAGIC) != magic)
    __builtin_abort ();
  if (fp1_constlref (magic) != magic)
    __builtin_abort ();
  if (fp1_constrref (MAGIC) != magic)
    __builtin_abort ();
}


// P0847R7
// { dg-do run { target c++23 } }

// explicit object member function pointer type deduction,
// conversion to function pointer,
// and calling through pointer to function

struct S {
  int _n;
  int f(this S& self) { return self._n; }
};

using f_type = int(*)(S&);

static_assert (__is_same (f_type, decltype (&S::f)));

int main()
{
  auto fp0 = &S::f;
  f_type fp1 = &S::f;
  static_assert (__is_same (decltype (fp0), decltype (fp1)));
  S s{42};
  if (fp0 (s) != 42)
    __builtin_abort ();
  if (fp1 (s) != 42)
    __builtin_abort ();
}


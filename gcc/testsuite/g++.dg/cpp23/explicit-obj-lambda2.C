// P0847R7
// { dg-do run { target c++23 } }

// recursive lambdas

inline constexpr int correct_result = 5 + 4 + 3 + 2 + 1; 

int main()
{
  auto cl0 = [](this auto&& self, int n)      -> int { return n ? self(n - 1) + n : 0; };
  auto cl1 = [](this auto const& self, int n) -> int { return n ? self(n - 1) + n : 0; };
  auto cl2 = [](this auto self, int n)        -> int { return n ? self(n - 1) + n : 0; };
  auto cl3 = [](this auto&& self, int n)     { if (!n) return 0; else return self(n - 1) + n; };
  auto cl4 = [](this auto const& self, int n){ if (!n) return 0; else return self(n - 1) + n; };
  auto cl5 = [](this auto self, int n)       { if (!n) return 0; else return self(n - 1) + n; };
  if (cl0(5) != correct_result) __builtin_abort ();
  if (cl1(5) != correct_result) __builtin_abort ();
  if (cl2(5) != correct_result) __builtin_abort ();
  if (cl3(5) != correct_result) __builtin_abort ();
  if (cl4(5) != correct_result) __builtin_abort ();
  if (cl5(5) != correct_result) __builtin_abort ();
}


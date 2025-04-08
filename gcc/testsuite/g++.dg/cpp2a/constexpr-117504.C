// PR c++/117504 - Initial report
// { dg-do "run" { target c++20 } }

struct span {
  span (const int (&__first)[1]) : _M_ptr (__first) {}
  int operator[] (long __i) { return _M_ptr[__i]; }
  const int *_M_ptr;
};

constexpr int a_global_vec[]{1};
span myFunctor() {
  return a_global_vec;
}

int main() {
  constexpr int a_vec[]{1};

  //
  // This PR's case, that used to be miscompiled.
  //
  auto lambda_1 = [&a_vec] () -> span { return a_vec; };
  auto vec_1 { lambda_1 () };
  if (vec_1[0] != 1)
    __builtin_abort ();

  // Variant that used to be miscompiled as well.
  auto lambda_2 = [&] () -> span { return a_vec; };
  auto vec_2 { lambda_2 () };
  if (vec_2[0] != 1)
    __builtin_abort ();

  //
  // Related cases that worked already.
  //
  auto lambda_3 = [&a_vec] () /* -> span */ { return a_vec; };
  auto vec_3 { lambda_3 () };
  if (vec_3[0] != 1)
    __builtin_abort ();

  auto lambda_4 = [&] () /* -> span */ { return a_vec; };
  auto vec_4 { lambda_4 () };
  if (vec_4[0] != 1)
    __builtin_abort ();

  const int (&vec_5)[1] = a_vec;
  if (vec_5[0] != 1)
    __builtin_abort ();
  
  span vec_6 (a_vec);
  if (vec_6[0] != 1)
    __builtin_abort ();

  auto vec_7 = myFunctor ();
  if (vec_7[0] != 1)
    __builtin_abort ();

  const int (&vec_8)[1] { a_vec };
  if (vec_8[0] != 1)
    __builtin_abort ();
}

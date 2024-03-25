// Verify we diagnose a call to a non-constant function pointer ahead of time.
// { dg-do compile { target c++11 } }

bool (*f)(int);

template<int N>
void g() {
  static_assert(f(N), ""); // { dg-error "non-constant|'f' is not usable" }
}

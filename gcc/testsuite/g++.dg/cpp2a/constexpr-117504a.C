// PR c++/117504 - ICE discovered by ppalka@ when reducing.
// { dg-do "compile" { target c++20 } }

struct span {
  span (const int* __first) : _M_ptr (__first) {}
  int operator[] (long __i) { return _M_ptr[__i]; }
  const int *_M_ptr;
};
int main() {
  constexpr int a_vec[]{1};
  auto vec { [&a_vec]() -> span { return a_vec; } () };
}

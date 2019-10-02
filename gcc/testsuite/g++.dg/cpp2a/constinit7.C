// PR c++/91360 - Implement C++20 P1143R2: constinit
// { dg-do compile { target c++11 } }

struct T {
  constexpr T(int) {}
  ~T();
};
__constinit T x = { 42 };
// ??? This should be rejected in C++14: copy initialization is not a constant
// expression on a non-literal type in C++14.  But 'constinit' is C++20 only.
__constinit T y = 42;

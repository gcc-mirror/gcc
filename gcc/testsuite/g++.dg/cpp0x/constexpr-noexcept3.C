// { dg-do compile { target c++11 } }

constexpr int f(int i) { return i; }
#define SA(X) static_assert (X, #X)
/* We used to assert that the following *is* noexcept, but this has changed
   in c++/87603.  */
SA(!noexcept(f(42)));
int j;
SA(!noexcept(f(j)));

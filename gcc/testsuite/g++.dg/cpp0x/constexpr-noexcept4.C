// { dg-do compile { target c++11 } }
// We used to treat a call to a constexpr function as noexcept if
// the call was a constant expression.  We no longer do since
// c++/87603.

#define SA(X) static_assert(X,#X)

constexpr const int* f(const int *p) { return p; }

int main()
{
  constexpr int i = 42;
  SA(!noexcept(*f(&i)));
  SA(!noexcept(f(&i)));
}

// { dg-do compile { target c++11 } }
// A call is noexcept if it is a valid subexpression of a constant
// expression, even if it is not itself a constant expression.

#define SA(X) static_assert(X,#X)

constexpr const int* f(const int *p) { return p; }

int main()
{
  constexpr int i = 42;
  SA(noexcept(*f(&i)));
  SA(noexcept(f(&i)));
}

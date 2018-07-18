// Test for deduction.
// { dg-options -std=c++17 }

template <class R, class... A>
void f(R (*)(A...));
void g(int) noexcept;

template <class R, class... A>
void h(R (*)(A...) noexcept);
void i(int);

int main()
{
  f(g);
  h(i);				// { dg-error "" }
}

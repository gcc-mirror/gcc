// Test that -Wnoexcept works with templates
// { dg-options "-std=c++11 -Wnoexcept" }

template <class T>
T f (T t) { return t; }		// { dg-warning "does not throw" }

#define SA(X) static_assert(X, #X)

SA (!noexcept(f(1)));		// { dg-warning "noexcept" }

int main()
{
  f(1);				// Use f(int) so it gets instantiated
}

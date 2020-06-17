// Test that -Wnoexcept works with templates
// { dg-do compile { target c++11 } }
// { dg-options "-Wnoexcept" }

template <class T>
T f (T t) { return t; }		// { dg-message "does not throw" }

#define SA(X) static_assert(X, #X)

SA (!noexcept(f(1)));		// { dg-warning "noexcept" }

int main()
{
  f(1);				// Use f(int) so it gets instantiated
}

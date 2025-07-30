// Test that the diagnostic mentions lack of constexpr
// { dg-do compile { target c++17 } }

template <auto f> void g() {}
void x()
{
  using fp = void (*)();
  fp f = nullptr;		// { dg-message "constexpr" }
  g<f>();			// { dg-error "" }
  int *p = nullptr;		// { dg-message "constexpr" }
  g<p>();			// { dg-error "" }
}

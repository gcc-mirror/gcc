// Test that we catch excessive recursion.
// { dg-options "-std=c++11 -fconstexpr-depth=5" }
// { dg-prune-output "in constexpr expansion" }
constexpr int f (int i) { return f (i-1); }
constexpr int i = f(42);	// { dg-error "constexpr evaluation depth" }

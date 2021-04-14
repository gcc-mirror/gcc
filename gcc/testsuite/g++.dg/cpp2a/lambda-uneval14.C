// PR c++/99478
// { dg-do compile { target c++20 } }

template <decltype ([] {})> auto f() {} // { dg-error "lambda" }

int main() { f<{}>(); }		// { dg-prune-output "no match" }

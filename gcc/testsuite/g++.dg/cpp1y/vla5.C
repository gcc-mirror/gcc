// PR c++/55149
// { dg-options -std=c++1y }

void test(int n) {
  int r[n];
  [&r]() { return r + 0; };
  [r]() { return r + 0; };	// { dg-error "captured by copy" }
}

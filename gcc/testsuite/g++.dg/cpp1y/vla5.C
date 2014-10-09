// PR c++/55149
// { dg-do compile { target c++14 } }

void test(int n) {
  int r[n];
  [&r]() { return r + 0; };
  [r]() { return r + 0; };	// { dg-error "captured by copy" }
}

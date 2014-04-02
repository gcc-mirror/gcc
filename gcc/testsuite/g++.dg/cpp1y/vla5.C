// PR c++/55149
// { dg-do compile { target c++1y } }

void test(int n) {
  int r[n];
  [&r]() { return r + 0; };
  [r]() { return r + 0; };	// { dg-error "captured by copy" }
}

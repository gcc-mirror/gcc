// Verify that builtin is used when declared in namespace std

// { dg-do compile }
// { dg-options "-Wall" }

namespace std {
  extern "C" int printf(const char*,...);
}

void foo() {
  std::printf("%d"); 		// { dg-warning "too few arguments" }
}

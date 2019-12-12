// Verify that builtin is used when declared in global namespace

// { dg-do compile }
// { dg-options "-Wall" }

extern "C" int printf(const char*,...);

void foo() {
  printf("%d"); 		// { dg-warning "12: expects a matching" }
}

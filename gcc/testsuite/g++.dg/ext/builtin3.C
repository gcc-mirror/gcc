// Verify that declaring builtin in namespace std doesn't give us
// declaration in global namespace

// { dg-do compile }
// { dg-options "" }

namespace std {
extern "C" int printf(char*, ...);
}

void foo() {
  printf("abc"); 		// { dg-error "not declared" }
}

// Verify that declaring builtin in namespace std doesn't give us
// declaration in global namespace

// { dg-do compile }
// { dg-options "" }

namespace std {
extern "C" int printf(char*, ...); // { dg-message "std::printf" }
}

void foo() {
  printf("abc"); // { dg-error "3:'printf' was not declared in this scope; did you mean 'std::printf'\\?" }
}

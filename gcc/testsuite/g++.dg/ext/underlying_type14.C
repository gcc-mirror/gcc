// Verify pretty-printing when nesting a builtin trait.

template<class T> void f(__underlying_type(__underlying_type(T))); // { dg-error "" }
// { dg-message "__underlying_type\\(__underlying_type\\(T\\)\\)\\)" "" { target *-*-* } .-1 }

int main() {
  f<int>(0); // { dg-error "no match" }
}

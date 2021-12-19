// { dg-do run }
// { dg-options "-Wconversion" }
// Similarly, we should be able to simultaneously reference bind to a temporary
// and cast away const in a single C-style cast.

#define assert(X) do { if (!(X)) __builtin_abort(); } while (0)

int main() {
  float f = 5.99f;
  int i = (int&) f; // { dg-warning "reference binding temporary" }
  // { dg-message "reinterpret_cast" "" { target *-*-* } .-1 }
  assert(i == 5);
}

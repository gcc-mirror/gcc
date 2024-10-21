// { dg-additional-options "-fmodules-ts -fmodule-lazy" }

import "class-9_a.H";
import "class-9_b.H";

int main() {
  // Lazy loading should still find the definitions of A and B.
  int a = foo()->a;
  int b = bar<int>()->b;
}

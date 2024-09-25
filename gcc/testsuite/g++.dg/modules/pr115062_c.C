// PR c++/115062
// { dg-additional-options "-fmodules-ts" }

import "pr115062_a.H";
import "pr115062_b.H";

int main() {
  X x = X() + "";
}

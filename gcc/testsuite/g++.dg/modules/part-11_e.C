// PR c++/124309
// { dg-additional-options "-fmodules" }

import Z;

int main() {
  frob(2);  // { dg-error "invalid conversion" }
}

// { dg-regexp "In module X, imported at \[^\n]*part-11_c.C:6,\n\\\s*included from \[^\n]*part-11_d.C:7,\nof module Z, imported at \[^\n]*part-11_e.C:4:\n\[^\n]*part-11_a.C:7:18: note:.*" }

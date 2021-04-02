// { dg-do preprocess }
// { dg-additional-options {-fdirectives-only -fmodules-ts} }
#include "pr99050_a.H"

int main () {}

// { dg-final { scan-file pr99050_b.i {import  "[^\n]*99050_a.H" \[\[__translated\]\];\n} }  }

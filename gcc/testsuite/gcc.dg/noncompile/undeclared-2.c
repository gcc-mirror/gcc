/* Invalid, but should not ICE.  PRs 11944, 14734.  */

void foo(const int[i]);  /* { dg-error "undeclared|for each" } */

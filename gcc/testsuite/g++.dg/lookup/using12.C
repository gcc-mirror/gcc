// PR c++/16707

int i;
using N::i; // { dg-error "declared|expected" }

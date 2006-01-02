// PR c++/16707

int i;
using N::i; // { dg-error "'N' has not been declared" }

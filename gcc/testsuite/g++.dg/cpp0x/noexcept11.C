// PR c++/50309
// { dg-options -std=c++0x }

void foo () noexcept () { } // { dg-error "expected" }

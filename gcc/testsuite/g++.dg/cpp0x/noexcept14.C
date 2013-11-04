// PR c++/50309
// { dg-options -std=c++11 }

void foo () noexcept () { } // { dg-error "expected" }

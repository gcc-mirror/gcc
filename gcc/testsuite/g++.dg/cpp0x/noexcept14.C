// PR c++/50309
// { dg-do compile { target c++11 } }

void foo () noexcept () { } // { dg-error "expected" }

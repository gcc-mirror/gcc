// PR c++/108080
// { dg-additional-options "-fmodules" }
// Give a diagnostic message rather than a crash for unsupported features.

[[gnu::optimize("-O3")]] void foo();  // { dg-warning "optimize" }

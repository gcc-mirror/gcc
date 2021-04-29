// Test digit separators in #line (bug 82359).  Test invalid usage.
// { dg-do preprocess { target c++14 } }

#line 0''123 // { dg-error "is not a positive integer" }

// PR c++/27315
// { dg-do compile }

template void operator+; // { dg-error "non-function" }

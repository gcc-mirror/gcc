// PR c++/27315
// { dg-do compile }

template void operator+; // { dg-error "15:declaration of .operator\\+. as non-function" }

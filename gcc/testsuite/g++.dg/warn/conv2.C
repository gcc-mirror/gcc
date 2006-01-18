// PR c++/13932
// { dg-options "-Wconversion" }

int i = 1.; // { dg-warning "converting" }

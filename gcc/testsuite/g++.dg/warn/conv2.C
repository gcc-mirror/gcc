// PR c++/13932
// { dg-options "-Wconversion" }

int i = 1.;
int j = 1.1; // { dg-warning "conversion" }

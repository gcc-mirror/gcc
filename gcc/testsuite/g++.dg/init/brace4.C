// PR c++/16859
// { dg-do compile }
// { dg-options "-pedantic" }

int a[] = { }; // { dg-error "zero-size array" }

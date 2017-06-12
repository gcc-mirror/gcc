// PR c++/79791
// { dg-do compile { target c++11 } }
// { dg-options "-Wno-error=write-strings -pedantic-errors" }

char *s = "foo"; // { dg-warning "ISO C\\+\\+ forbids converting a string constant" }

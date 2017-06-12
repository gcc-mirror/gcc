// PR c++/79791
// { dg-do compile { target { ! c++11 } } }
// { dg-options "-pedantic-errors" }

char *s = "foo"; // { dg-warning "deprecated conversion from string constant" }

// PR c++/79791
// { dg-do compile { target c++11 } }
// { dg-options "-Werror=write-strings -Wpedantic" }
// { dg-message "some warnings being treated as errors" "" { target *-*-* } 0 }

char *s = "foo"; // { dg-error "ISO C\\+\\+ forbids converting a string constant" }

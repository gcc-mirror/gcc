// PR c++/79791
// { dg-do compile { target { ! c++11 } } }
// { dg-options "-Werror=write-strings" }
// { dg-message "some warnings being treated as errors" "" { target *-*-* } 0 }

char *s = "foo"; // { dg-error "deprecated conversion from string constant" }

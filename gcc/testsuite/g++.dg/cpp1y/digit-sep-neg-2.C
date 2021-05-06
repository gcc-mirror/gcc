// Test adjacent digit separators rejected in exponent (bug 83873).
// { dg-do compile { target c++14 } }

double d = 1.0e1''0; /* { dg-error "adjacent digit separators" } */

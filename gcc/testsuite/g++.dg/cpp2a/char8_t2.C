// P0482R6
// { dg-do compile }
// { dg-options "-std=c++2a -fno-char8_t" }

char8_t c8; // { dg-error "does not name a type" }

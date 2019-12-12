// Test that an error is issued for attempted char8_t typedef declarations
// when -fchar8_t is enabled.
// { dg-do compile }
// { dg-options "-fchar8_t" }

typedef unsigned char char8_t; // { dg-error "redeclaration" }

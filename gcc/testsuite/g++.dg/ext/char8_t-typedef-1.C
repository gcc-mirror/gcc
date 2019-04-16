// Test that no error is issued for attempted char8_t typedef declarations
// when -fchar8_t is not enabled.
// { dg-do compile }
// { dg-options "-fno-char8_t" }

typedef unsigned char char8_t;

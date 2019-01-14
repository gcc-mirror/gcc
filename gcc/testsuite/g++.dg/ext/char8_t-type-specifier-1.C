// Test that char8_t is not a type specifier if -fchar8_t is not enabled.
// { dg-do compile }
// { dg-options "-fno-char8_t" }

char8_t c8; /* { dg-error ".char8_t. does not name a type" "no-char8_t" } */

// Test that char8_t is not a keyword if -fchar8_t is not enabled.
// { dg-do compile }
// { dg-options "-fno-char8_t" }

int char8_t;

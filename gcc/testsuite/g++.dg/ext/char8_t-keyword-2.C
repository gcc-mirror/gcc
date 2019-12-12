// Test that char8_t is recognized as a keyword if -fchar8_t is enabled.
// { dg-do compile }
// { dg-options "-fchar8_t" }

int char8_t; /* { dg-error "multiple types in one declaration|declaration does not declare anything" "char8_t" } */

// Test for unsignedness and that the max limit of char8_t is at least 0xFF
// when -fchar8_t is enabled.
// { dg-do compile }
// { dg-options "-std=c++17 -fchar8_t" }

static_assert(u8'\xFF' == 0xFF, "Error");
static_assert(u8"\xFF"[0] == 0xFF, "Error");
static_assert(char8_t(-1) >= 0, "Error");
static_assert(char8_t{-1} >= 0, "Error"); // { dg-error "narrowing conversion of .-1. from .int. to .char8_t." "char8_t" }

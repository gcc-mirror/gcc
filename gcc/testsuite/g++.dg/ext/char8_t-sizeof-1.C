// Test sizeof for char8_t.
// { dg-do compile }
// { dg-options "-std=c++17 -fchar8_t" }

static_assert(sizeof(u8'x') == 1);
static_assert(sizeof(char8_t) == 1);
static_assert(sizeof(__CHAR8_TYPE__) == 1);

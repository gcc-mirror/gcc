// Test that predefined feature test macros are not present when -fchar8_t is
// not enabled.
// { dg-do compile }
// { dg-options "-fno-char8_t" }

#if defined(__cpp_char8_t)
#error __cpp_char8_t is defined!
#endif

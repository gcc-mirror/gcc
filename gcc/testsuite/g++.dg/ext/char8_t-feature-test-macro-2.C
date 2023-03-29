// Test that predefined feature test macros are present when -fchar8_t is
// enabled.
// { dg-do compile }
// { dg-options "-fchar8_t" }

#if !defined(__cpp_char8_t)
#  error __cpp_char8_t is not defined!
#elif __cpp_char8_t != 202207
#  error __cpp_char8_t != 202207
#endif

/* Test silencing C++20 va_opt pedwarns.  */

#pragma GCC diagnostic push

void f()
{
#define MAC1(FMT, ...) __builtin_printf ((FMT) __VA_OPT__(,) __VA_ARGS__)
  /* { dg-error "variadic macro" "" { target { c++98_only } } .-1 } */
  /* { dg-error "VA_OPT" "" { target { c++17_down } } .-2 } */
  MAC1("foo"); /* { dg-error "empty macro arguments" "" { target c++98_only } } */
  /* { dg-error "at least one argument" "" { target c++17_down } .-1 } */

#pragma GCC diagnostic ignored "-Wvariadic-macros"
#pragma GCC diagnostic ignored "-Wc++20-extensions"

#define MAC2(FMT, ...) __builtin_printf ((FMT) __VA_OPT__(,) __VA_ARGS__)
  MAC2("foo");
}

// Test digit separators in #line (bug 82359).
// { dg-do compile { target c++14 } }

#line 0'123
static_assert (__LINE__ == 123, "#line with digit separator");

#line 4'56'7'8'9
static_assert (__LINE__ == 456789, "#line with digit separator");

/* Test invalid initializers that are consistent with the syntax: undefined
   behavior ("shall" in Semantics not Constraints) before C2y, constraint
   violation in C2y.  Array cases.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

typedef __WCHAR_TYPE__ wchar_t;
typedef __CHAR8_TYPE__ char8_t;
typedef __CHAR16_TYPE__ char16_t;
typedef __CHAR32_TYPE__ char32_t;

const char c1[] = "", c2[] = { "" }, c3[] = { "", };
char c4[] = u8"", c5[] = { u8"" }, c6[] = { u8"", };

signed char sc1[] = "", sc2[] = { "" }, sc3[] = { "", };
volatile signed char sc4[] = u8"", sc5[] = { u8"" }, sc6[] = { u8"", };

unsigned char uc1[] = "", uc2[] = { "" }, uc3[] = { "", };
unsigned char uc4[] = u8"", uc5[] = { u8"" }, uc6[] = { u8"", };

char8_t c8_1[] = "", c8_2[] = { "" }, c8_3[] = { "", };
char8_t c8_4[] = u8"", c8_5[] = { u8"" }, c8_6[] = { u8"", };

wchar_t w1[] = L"", w2[] = { L"" }, w3[] = { L"", };
char16_t c16_1[] = u"", c16_2[] = { u"" }, c16_3[] = { u"", };
char32_t c32_1[] = U"", c32_2[] = { U"" }, c32_3[] = { U"", };

int ia[] = { 1, 2, 3 };

_Atomic char ac[] = ""; /* { dg-error "inappropriate type" } */
_Atomic wchar_t aw[] = L""; /* { dg-error "inappropriate type" } */
_Atomic char8_t ac8[] = u8""; /* { dg-error "inappropriate type" } */
_Atomic char16_t ac16[] = u""; /* { dg-error "inappropriate type" } */
_Atomic char32_t ac32[] = U""; /* { dg-error "inappropriate type" } */

#if __WCHAR_WIDTH__ > __SCHAR_WIDTH__
typedef char char_not_wchar;
typedef wchar_t wchar_not_char;
#else
typedef long long int char_not_wchar;
typedef long long int wchar_not_char;
#endif
char_not_wchar cnw[] = L""; /* { dg-error "cannot initialize|inappropriate type" } */
char_not_wchar cnwb[] = { L"" }; /* { dg-error "cannot initialize|inappropriate type" } */
wchar_not_char wnc[] = ""; /* { dg-error "cannot initialize|inappropriate type" } */
wchar_not_char wncb[] = { "" }; /* { dg-error "cannot initialize|inappropriate type" } */
wchar_not_char wnc8[] = u8""; /* { dg-error "cannot initialize|inappropriate type" } */
wchar_not_char wnc8b[] = { u8"" }; /* { dg-error "cannot initialize|inappropriate type" } */

#if __INT_LEAST16_WIDTH__ > __SCHAR_WIDTH__
typedef char char_not_char16;
typedef char16_t char16_not_char;
#else
typedef long long int char_not_char16;
typedef long long int char16_not_char;
#endif
char_not_char16 cn16[] = u""; /* { dg-error "cannot initialize|inappropriate type" } */
char_not_char16 cn16b[] = { u"" }; /* { dg-error "cannot initialize|inappropriate type" } */
char16_not_char c16nc[] = ""; /* { dg-error "cannot initialize|inappropriate type" } */
char16_not_char c16ncb[] = { "" }; /* { dg-error "cannot initialize|inappropriate type" } */
char16_not_char c16nc8[] = u8""; /* { dg-error "cannot initialize|inappropriate type" } */
char16_not_char c16nc8b[] = { u8"" }; /* { dg-error "cannot initialize|inappropriate type" } */

#if __INT_LEAST32_WIDTH__ > __SCHAR_WIDTH__
typedef char char_not_char32;
typedef char32_t char32_not_char;
#else
typedef long long int char_not_char32;
typedef long long int char32_not_char;
#endif
char_not_char32 cn32[] = U""; /* { dg-error "cannot initialize|inappropriate type" } */
char_not_char32 cn32b[] = { U"" }; /* { dg-error "cannot initialize|inappropriate type" } */
char32_not_char c32nc[] = ""; /* { dg-error "cannot initialize|inappropriate type" } */
char32_not_char c32ncb[] = { "" }; /* { dg-error "cannot initialize|inappropriate type" } */
char32_not_char c32nc8[] = u8""; /* { dg-error "cannot initialize|inappropriate type" } */
char32_not_char c32nc8b[] = { u8"" }; /* { dg-error "cannot initialize|inappropriate type" } */

#if __WCHAR_WIDTH__ == __INT_LEAST16_WIDTH__
typedef long long int wchar_not_char16;
typedef long long int char16_not_wchar;
#else
typedef wchar_t wchar_not_char16;
typedef char16_t char16_not_wchar;
#endif
wchar_not_char16 wcn16[] = u""; /* { dg-error "cannot initialize|inappropriate type" } */
wchar_not_char16 wcn16b[] = { u"" }; /* { dg-error "cannot initialize|inappropriate type" } */
char16_not_wchar c16nwc[] = L""; /* { dg-error "cannot initialize|inappropriate type" } */
char16_not_wchar c16nwcb[] = { L"" }; /* { dg-error "cannot initialize|inappropriate type" } */

#if __WCHAR_WIDTH__ == __INT_LEAST32_WIDTH__
typedef long long int wchar_not_char32;
typedef long long int char32_not_wchar;
#else
typedef wchar_t wchar_not_char32;
typedef char32_t char32_not_wchar;
#endif
wchar_not_char32 wcn32[] = U""; /* { dg-error "cannot initialize|inappropriate type" } */
wchar_not_char32 wcn32b[] = { U"" }; /* { dg-error "cannot initialize|inappropriate type" } */
char32_not_wchar c32nwc[] = L""; /* { dg-error "cannot initialize|inappropriate type" } */
char32_not_wchar c32nwcb[] = { L"" }; /* { dg-error "cannot initialize|inappropriate type" } */

void
f ()
{
  int ic[] = ia; /* { dg-error "invalid initializer" } */
}

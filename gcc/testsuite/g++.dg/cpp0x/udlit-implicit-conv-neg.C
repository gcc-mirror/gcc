// { dg-options -std=c++0x }

#include <cstdint>

int operator"" _bar (long double);

double operator"" _foo (long long unsigned);

int i = 12_bar; // { dg-error "unable to find numeric literal operator|with|argument" }

double d = 1.2_foo; // { dg-error "unable to find numeric literal operator|with|argument" }

int operator"" _char(char);

int operator"" _wchar_t(wchar_t);

int operator"" _char16_t(char16_t);

int operator"" _char32_t(char32_t);

int cwcx = 'c'_wchar_t; // { dg-error "unable to find character literal operator|with|argument" }
int cc16 = 'c'_char16_t; // { dg-error "unable to find character literal operator|with|argument" }
int cc32 = 'c'_char32_t; // { dg-error "unable to find character literal operator|with|argument" }

int wccx = L'c'_char; // { dg-error "unable to find character literal operator|with|argument" }
int wcc16 = L'c'_char16_t; // { dg-error "unable to find character literal operator|with|argument" }
int wcc32 = L'c'_char32_t; // { dg-error "unable to find character literal operator|with|argument" }

int c16c = u'c'_char; // { dg-error "unable to find character literal operator|with|argument" }
int c16wc = u'c'_wchar_t; // { dg-error "unable to find character literal operator|with|argument" }
int c16c32 = u'c'_char32_t; // { dg-error "unable to find character literal operator|with|argument" }

int c32c = U'c'_char; // { dg-error "unable to find character literal operator|with|argument" }
int c32wc = U'c'_wchar_t; // { dg-error "unable to find character literal operator|with|argument" }
int c32c16 = U'c'_char16_t; // { dg-error "unable to find character literal operator|with|argument" }

int operator"" _char_str(const char*, std::size_t);

int operator"" _wchar_t_str(const wchar_t*, std::size_t);

int operator"" _char16_t_str(const char16_t*, std::size_t);

int operator"" _char32_t_str(const char32_t*, std::size_t);

int strwstr = "str"_wchar_t_str; // { dg-error "unable to find string literal operator|with|arguments" }
int strstr16 = "str"_char16_t_str; // { dg-error "unable to find string literal operator|with|arguments" }
int strstr32 = "str"_char32_t_str; // { dg-error "unable to find string literal operator|with|arguments" }

int str8wstr = u8"str"_wchar_t_str; // { dg-error "unable to find string literal operator|with|arguments" }
int str8str16 = u8"str"_char16_t_str; // { dg-error "unable to find string literal operator|with|arguments" }
int str8str32 = u8"str"_char32_t_str; // { dg-error "unable to find string literal operator|with|arguments" }

int wstrstr = L"str"_char_str; // { dg-error "unable to find string literal operator|with|arguments" }
int wstrstr16 = L"str"_char16_t_str; // { dg-error "unable to find string literal operator|with|arguments" }
int wstrstr32 = L"str"_char32_t_str; // { dg-error "unable to find string literal operator|with|arguments" }

int str16str = u"str"_char_str; // { dg-error "unable to find string literal operator|with|arguments" }
int str16wstr = u"str"_wchar_t_str; // { dg-error "unable to find string literal operator|with|arguments" }
int str16str32 = u"str"_char32_t_str; // { dg-error "unable to find string literal operator|with|arguments" }

int str32str = U"str"_char_str; // { dg-error "unable to find string literal operator|with|arguments" }
int str32wstr = U"str"_wchar_t_str; // { dg-error "unable to find string literal operator|with|arguments" }
int str32str16 = U"str"_char16_t_str; // { dg-error "unable to find string literal operator string operator|with|arguments" }

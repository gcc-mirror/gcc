// C++26 P1854R4 - Making non-encodable string literals ill-formed
// { dg-do compile { target c++11 } }
// { dg-require-effective-target int32 }
// { dg-options "-pedantic-errors -finput-charset=UTF-8 -fexec-charset=ISO-8859-1" }
/* { dg-require-iconv "ISO-8859-1" } */

int a = 'abcd';						// { dg-warning "multi-character character constant" }
int b = '\x61\x62\x63\x64';				// { dg-warning "multi-character character constant" }
int c = 'á';
int d = '😁';						// { dg-error "converting to execution character set" }
int e = '\N{FACE WITH TEARS OF JOY}';			// { dg-error "converting UCN to execution character set" }
							// { dg-error "named universal character escapes are only valid in" "" { target c++20_down } .-1 }
int f = '\U0001F602';					// { dg-error "converting UCN to execution character set" }
wchar_t g = L'abcd';					// { dg-error "multi-character literal cannot have an encoding prefix" "" { target c++23 } }
							// { dg-warning "multi-character literal cannot have an encoding prefix" "" { target c++20_down } .-1 }
wchar_t h = L'\x61\x62\x63\x64';			// { dg-error "multi-character literal cannot have an encoding prefix" "" { target c++23 } }
							// { dg-warning "multi-character literal cannot have an encoding prefix" "" { target c++20_down } .-1 }
wchar_t i = L'á';
char16_t j = u'abcd';					// { dg-error "multi-character literal cannot have an encoding prefix" }
char16_t k = u'\x61\x62\x63\x64';			// { dg-error "multi-character literal cannot have an encoding prefix" }
char16_t l = u'á';
char16_t m = u'😁';					// { dg-error "character not encodable in a single code unit" }
char16_t n = u'\N{FACE WITH TEARS OF JOY}';		// { dg-error "character not encodable in a single code unit" { target c++23 } }
							// { dg-error "named universal character escapes are only valid in" "" { target c++20_down } .-1 }
char16_t o = u'\U0001F602';				// { dg-error "character not encodable in a single code unit" }
char32_t p = U'abcd';					// { dg-error "multi-character literal cannot have an encoding prefix" }
char32_t q = U'\x61\x62\x63\x64';			// { dg-error "multi-character literal cannot have an encoding prefix" }
char32_t r = U'á';
char32_t s = U'😁';
char32_t t = U'\N{FACE WITH TEARS OF JOY}';		// { dg-error "named universal character escapes are only valid in" "" { target c++20_down } }
char32_t u = U'\U0001F602';
#if __cpp_unicode_characters >= 201411L
auto v = u8'abcd';					// { dg-error "multi-character literal cannot have an encoding prefix" "" { target c++17 } }
auto w = u8'\x61\x62\x63\x64';				// { dg-error "multi-character literal cannot have an encoding prefix" "" { target c++17 } }
auto x = u8'á';						// { dg-error "character not encodable in a single code unit" "" { target c++17 } }
auto y = u8'😁';					// { dg-error "character not encodable in a single code unit" "" { target c++17 } }
auto z = u8'\N{FACE WITH TEARS OF JOY}';		// { dg-error "character not encodable in a single code unit" "" { target c++17 } }
							// { dg-error "named universal character escapes are only valid in" "" { target { c++17 && c++20_down } } .-1 }
auto aa = u8'\U0001F602';				// { dg-error "character not encodable in a single code unit" "" { target c++17 } }
#endif
const char *ab = "😁";					// { dg-error "converting to execution character set" }
const char *ac = "\N{FACE WITH TEARS OF JOY}";		// { dg-error "converting UCN to execution character set" }
							// { dg-error "named universal character escapes are only valid in" "" { target c++20_down } .-1 }
const char *ad = "\U0001F602";				// { dg-error "converting UCN to execution character set" }
const char16_t *ae = u"😁";
const char16_t *af = u"\N{FACE WITH TEARS OF JOY}";	// { dg-error "named universal character escapes are only valid in" "" { target c++20_down } }
const char16_t *ag = u"\U0001F602";
const char32_t *ah = U"😁";
const char32_t *ai = U"\N{FACE WITH TEARS OF JOY}";	// { dg-error "named universal character escapes are only valid in" "" { target c++20_down } }
const char32_t *aj = U"\U0001F602";
auto ak = u8"😁";
auto al = u8"\N{FACE WITH TEARS OF JOY}";		// { dg-error "named universal character escapes are only valid in" "" { target c++20_down } }
auto am = u8"\U0001F602";
int an = '\x123456789';					// { dg-error "hex escape sequence out of range" }
wchar_t ao = L'\x123456789abcdef0';			// { dg-error "hex escape sequence out of range" }
char16_t ap = u'\x12345678';				// { dg-error "hex escape sequence out of range" }
char32_t aq = U'\x123456789abcdef0';			// { dg-error "hex escape sequence out of range" }
#if __cpp_unicode_characters >= 201411L
auto ar = u8'\x123456789abcdef0';			// { dg-error "hex escape sequence out of range" "" { target c++17 } }
#endif
char as = '\xff';
#if __SIZEOF_WCHAR_T__ * __CHAR_BIT__ == 32
wchar_t at = L'\xffffffff';
#elif __SIZEOF_WCHAR_T__ * __CHAR_BIT__ == 16
wchar_t at = L'\xffff';
#endif
int au = '\x1234';					// { dg-error "hex escape sequence out of range" }
int av = 'abcdefghijklmnop';				// { dg-warning "multi-character literal with \[0-9]+ characters exceeds 'int' size of \[0-9]+ bytes" }

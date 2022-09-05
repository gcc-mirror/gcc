// P2362R3 - Remove non-encodable wide character literals and multicharacter
// wide character literals.
// { dg-do compile }

char a = 'a';
int b = 'ab';			// { dg-warning "multi-character character constant" }
int c = '\u05D9';		// { dg-warning "multi-character character constant" }
#if __SIZEOF_INT__ > 2
int d = '\U0001F525';		// { dg-warning "multi-character character constant" "" { target int32 } }
#endif
int e = 'abcd';			// { dg-warning "multi-character character constant" }
wchar_t f = L'f';
wchar_t g = L'gh';		// { dg-error "character constant too long for its type" "" { target c++23 } }
				// { dg-warning "character constant too long for its type" "" { target c++20_down } .-1 }
wchar_t h = L'ijkl';		// { dg-error "character constant too long for its type" "" { target c++23 } }
				// { dg-warning "character constant too long for its type" "" { target c++20_down } .-1 }
wchar_t i = L'\U0001F525';	// { dg-error "character constant too long for its type" "" { target { c++23 && { ! 4byte_wchar_t } } } }
				// { dg-warning "character constant too long for its type" "" { target { c++20_down && { ! 4byte_wchar_t } } } .-1 }
#ifdef __cpp_char8_t
typedef char8_t u8;
#else
typedef char u8;
#endif
#if __cpp_unicode_characters >= 201411 
u8 j = u8'j';
u8 k = u8'kl';			// { dg-error "character constant too long for its type" "" { target c++17 } }
u8 l = u8'\U0001F525';		// { dg-error "character constant too long for its type" "" { target c++17 }  }
#endif
#if __cpp_unicode_characters >= 200704
char16_t m = u'm';
char16_t n = u'no';		// { dg-error "character constant too long for its type" "" { target c++11 } }
char16_t o = u'\u05D9';
char16_t p = u'\U0001F525';	// { dg-error "character constant too long for its type" "" { target c++11 } }
char32_t q = U'm';
char32_t r = U'no';		// { dg-error "character constant too long for its type" "" { target c++11 } }
char32_t s = U'\u05D9';
char32_t t = U'\U0001F525';
#endif
wchar_t u = L'\u0065\u0301';		// { dg-error "character constant too long for its type" "" { target c++23 } }
				// { dg-warning "character constant too long for its type" "" { target c++20_down } .-1 }
wchar_t v = L'é';		// { dg-error "character constant too long for its type" "" { target c++23 } }
				// { dg-warning "character constant too long for its type" "" { target c++20_down } .-1 }

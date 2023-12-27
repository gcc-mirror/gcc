// P2362R3 - Remove non-encodable wide character literals and multicharacter
// wide character literals.
// { dg-do compile }
// { dg-require-effective-target 4byte_wchar_t }

char a = 'a';
int b = 'ab';			// { dg-warning "multi-character character constant" }
int c = '\u05D9';		// { dg-error "character not encodable in a single execution character code unit" }
#if __SIZEOF_INT__ > 2
int d = '\U0001F525';		// { dg-error "character not encodable in a single execution character code unit" "" { target int32 } }
#endif
int e = 'abcd';			// { dg-warning "multi-character character constant" "" { target int32plus } }
				// { dg-warning "multi-character literal with \[0-9]+ characters exceeds 'int' size of \[0-9]+ bytes" "" { target { ! int32plus } } .-1 }
wchar_t f = L'f';
wchar_t g = L'gh';		// { dg-error "multi-character literal cannot have an encoding prefix" "" { target c++23 } }
				// { dg-warning "multi-character literal cannot have an encoding prefix" "" { target c++20_down } .-1 }
wchar_t h = L'ijkl';		// { dg-error "multi-character literal cannot have an encoding prefix" "" { target c++23 } }
				// { dg-warning "multi-character literal cannot have an encoding prefix" "" { target c++20_down } .-1 }
wchar_t i = L'\U0001F525';	// { dg-error "multi-character literal cannot have an encoding prefix" "" { target { c++23 && { ! 4byte_wchar_t } } } }
				// { dg-warning "multi-character literal cannot have an encoding prefix" "" { target { c++20_down && { ! 4byte_wchar_t } } } .-1 }
#ifdef __cpp_char8_t
typedef char8_t u8;
#else
typedef char u8;
#endif
#if __cpp_unicode_characters >= 201411 
u8 j = u8'j';
u8 k = u8'kl';			// { dg-error "multi-character literal cannot have an encoding prefix" "" { target c++17 } }
u8 l = u8'\U0001F525';		// { dg-error "character not encodable in a single code unit" "" { target c++17 }  }
#endif
#if __cpp_unicode_characters >= 200704
char16_t m = u'm';
char16_t n = u'no';		// { dg-error "multi-character literal cannot have an encoding prefix" "" { target c++11 } }
char16_t o = u'\u05D9';
char16_t p = u'\U0001F525';	// { dg-error "character not encodable in a single code unit" "" { target c++11 } }
char32_t q = U'm';
char32_t r = U'no';		// { dg-error "multi-character literal cannot have an encoding prefix" "" { target c++11 } }
char32_t s = U'\u05D9';
char32_t t = U'\U0001F525';
#endif
wchar_t u = L'\u0065\u0301';		// { dg-error "multi-character literal cannot have an encoding prefix" "" { target c++23 } }
				// { dg-warning "multi-character literal cannot have an encoding prefix" "" { target c++20_down } .-1 }
wchar_t v = L'é';		// { dg-error "multi-character literal cannot have an encoding prefix" "" { target c++23 } }
				// { dg-warning "multi-character literal cannot have an encoding prefix" "" { target c++20_down } .-1 }

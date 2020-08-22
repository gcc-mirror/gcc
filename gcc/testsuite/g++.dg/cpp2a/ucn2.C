// P1139R2
// { dg-do compile { target c++11 } }
// { dg-additional-options "-fchar8_t" { target c++17_down } }

const char16_t *a = u"\U0001F914\u2753";
const char32_t *b = U"\U0001F914\u2753";
const char16_t *c = u"\uD802";		// { dg-error "is not a valid universal character" }
const char16_t *d = u"\U0000DFF0";	// { dg-error "is not a valid universal character" }
const char16_t *e = u"\U00110000";	// { dg-error "is outside the UCS codespace" "" { target c++20 } }
					// { dg-error "converting UCN to execution character set" "" { target *-*-* } .-1 }
const char32_t *f = U"\uD802";		// { dg-error "is not a valid universal character" }
const char32_t *g = U"\U0000DFF0";	// { dg-error "is not a valid universal character" }
const char32_t *h = U"\U00110001";	// { dg-error "is outside the UCS codespace" "" { target c++20 } }
#if __cpp_unicode_characters >= 201411
const char8_t i = u8'\u00C0';		// { dg-error "character constant too long for its type" "" { target c++17 } }
#endif
const char16_t j = u'\U0001F914';	// { dg-error "character constant too long for its type" }
const char32_t k = U'\U0001F914';
#if __cpp_unicode_characters >= 201411
const char8_t l = u8'ab';		// { dg-error "character constant too long for its type" "" { target c++17 } }
#endif
const char16_t m = u'ab';		// { dg-error "character constant too long for its type" }
const char32_t n = U'ab';		// { dg-error "character constant too long for its type" }
#if __cpp_unicode_characters >= 201411
const char8_t o = u8'\U00110002';	// { dg-error "is outside the UCS codespace" "" { target c++20 } }
					// { dg-error "character constant too long for its type" "" { target c++17 } .-1 }
#endif
const char16_t p = u'\U00110003';	// { dg-error "is outside the UCS codespace" "" { target c++20 } }
					// { dg-error "converting UCN to execution character set" "" { target *-*-* } .-1 }
const char32_t q = U'\U00110004';	// { dg-error "is outside the UCS codespace" "" { target c++20 } }

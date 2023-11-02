// C++26 P2361R6 - Unevaluated strings
// { dg-do compile { target { c++11 && c++23_down } } }
// { dg-options "-pedantic" }

static_assert (true, "foo");
static_assert (true, "foo" " " "bar");
static_assert (true, "\u01FC\U000001FC\u{1FC}\N{LATIN CAPITAL LETTER AE WITH ACUTE}\\\'\"\?\a\b\f\n\r\t\v");
// { dg-warning "delimited escape sequences are only valid in" "" { target c++20_down } .-1 }
// { dg-warning "named universal character escapes are only valid in" "" { target c++20_down } .-2 }
static_assert (true, L"foo");
static_assert (true, u"foo");
static_assert (true, U"foo");
static_assert (true, u8"foo");
static_assert (true, L"fo" "o");
static_assert (true, u"fo" "o");
static_assert (true, U"fo" "o");
static_assert (true, u8"fo" "o");
static_assert (true, "fo" L"o");
static_assert (true, "fo" u"o");
static_assert (true, "fo" U"o");
static_assert (true, "fo" u8"o");
static_assert (true, "\0");
static_assert (true, "\17");
static_assert (true, "\x20");
static_assert (true, "\o{17}");		// { dg-warning "delimited escape sequences are only valid in" "" { target c++20_down } }
static_assert (true, "\x{20}");		// { dg-warning "delimited escape sequences are only valid in" "" { target c++20_down } }
static_assert (true, "\h");		// { dg-warning "unknown escape sequence" }

extern "C" "+" "+" int f0 ();
extern "C" int f1 ();
extern "C" { int f2 (); };
extern L"C" int f3 ();			// { dg-error "a wide string is invalid in this context" }
extern L"C" { int f4 (); }		// { dg-error "a wide string is invalid in this context" }
extern u"C" int f5 ();			// { dg-error "a wide string is invalid in this context" }
extern u"C" { int f6 (); }		// { dg-error "a wide string is invalid in this context" }
extern U"C" int f7 ();			// { dg-error "a wide string is invalid in this context" }
extern U"C" { int f8 (); }		// { dg-error "a wide string is invalid in this context" }
extern u8"C" int f9 ();			// { dg-error "a wide string is invalid in this context" }
extern u8"C" { int f10 (); }		// { dg-error "a wide string is invalid in this context" }
extern "\x43" int f11 ();
extern "\x{43}" { int f12 (); }		// { dg-warning "delimited escape sequences are only valid in" "" { target c++20_down } }
extern "\103" int f13 ();
extern "\o{0103}" { int f14 (); }	// { dg-warning "delimited escape sequences are only valid in" "" { target c++20_down } }

[[deprecated ("foo")]] int g0 ();
[[deprecated ("foo" " " "bar")]] int g1 ();
[[deprecated ("\u01FC\U000001FC\u{1FC}\N{LATIN CAPITAL LETTER AE WITH ACUTE}\\\'\"\?\a\b\f\n\r\t\v")]] int g2 ();
// { dg-warning "delimited escape sequences are only valid in" "" { target c++20_down } .-1 }
// { dg-warning "named universal character escapes are only valid in" "" { target c++20_down } .-2 }
[[deprecated (L"foo")]] int g3 ();
[[deprecated (u"foo")]] int g4 ();
[[deprecated (U"foo")]] int g5 ();
[[deprecated (u8"foo")]] int g6 ();
[[deprecated (L"fo" "o")]] int g7 ();
[[deprecated (u"fo" "o")]] int g8 ();
[[deprecated (U"fo" "o")]] int g9 ();
[[deprecated (u8"fo" "o")]] int g10 ();
[[deprecated ("fo" L"o")]] int g11 ();
[[deprecated ("fo" u"o")]] int g12 ();
[[deprecated ("fo" U"o")]] int g13 ();
[[deprecated ("fo" u8"o")]] int g14 ();
[[deprecated ("\0")]] int g15 ();
[[deprecated ("\17")]] int g16 ();
[[deprecated ("\x20")]] int g17 ();
[[deprecated ("\o{17}")]] int g18 ();	// { dg-warning "delimited escape sequences are only valid in" "" { target c++20_down } }
[[deprecated ("\x{20}")]] int g19 ();	// { dg-warning "delimited escape sequences are only valid in" "" { target c++20_down } }
[[deprecated ("\h")]] int g20 ();	// { dg-warning "unknown escape sequence" }

[[nodiscard ("foo")]] int h0 ();
[[nodiscard ("foo" " " "bar")]] int h1 ();
[[nodiscard ("\u01FC\U000001FC\u{1FC}\N{LATIN CAPITAL LETTER AE WITH ACUTE}\\\'\"\?\a\b\f\n\r\t\v")]] int h2 ();
// { dg-warning "delimited escape sequences are only valid in" "" { target c++20_down } .-1 }
// { dg-warning "named universal character escapes are only valid in" "" { target c++20_down } .-2 }
[[nodiscard (L"foo")]] int h3 ();
[[nodiscard (u"foo")]] int h4 ();
[[nodiscard (U"foo")]] int h5 ();
[[nodiscard (u8"foo")]] int h6 ();
[[nodiscard (L"fo" "o")]] int h7 ();
[[nodiscard (u"fo" "o")]] int h8 ();
[[nodiscard (U"fo" "o")]] int h9 ();
[[nodiscard (u8"fo" "o")]] int h10 ();
[[nodiscard ("fo" L"o")]] int h11 ();
[[nodiscard ("fo" u"o")]] int h12 ();
[[nodiscard ("fo" U"o")]] int h13 ();
[[nodiscard ("fo" u8"o")]] int h14 ();
[[nodiscard ("\0")]] int h15 ();
[[nodiscard ("\17")]] int h16 ();
[[nodiscard ("\x20")]] int h17 ();
[[nodiscard ("\o{17}")]] int h18 ();	// { dg-warning "delimited escape sequences are only valid in" "" { target c++20_down } }
[[nodiscard ("\x{20}")]] int h19 ();	// { dg-warning "delimited escape sequences are only valid in" "" { target c++20_down } }
[[nodiscard ("\h")]] int h20 ();	// { dg-warning "unknown escape sequence" }

float operator "" _my0 (const char *);
float operator "" "" _my1 (const char *);
float operator L"" _my2 (const char *);	// { dg-error "invalid encoding prefix in literal operator" }
float operator u"" _my3 (const char *);	// { dg-error "invalid encoding prefix in literal operator" }
float operator U"" _my4 (const char *);	// { dg-error "invalid encoding prefix in literal operator" }
float operator u8"" _my5 (const char *);	// { dg-error "invalid encoding prefix in literal operator" }
float operator L"" "" _my6 (const char *);	// { dg-error "invalid encoding prefix in literal operator" }
float operator u"" "" _my7 (const char *);	// { dg-error "invalid encoding prefix in literal operator" }
float operator U"" "" _my8 (const char *);	// { dg-error "invalid encoding prefix in literal operator" }
float operator u8"" "" _my9 (const char *);	// { dg-error "invalid encoding prefix in literal operator" }
float operator "" L"" _my10 (const char *);	// { dg-error "invalid encoding prefix in literal operator" }
float operator "" u"" _my11 (const char *);	// { dg-error "invalid encoding prefix in literal operator" }
float operator "" U"" _my12 (const char *);	// { dg-error "invalid encoding prefix in literal operator" }
float operator "" u8"" _my13 (const char *);	// { dg-error "invalid encoding prefix in literal operator" "" { target c++20 } }
float operator "\0" _my14 (const char *);	// { dg-error "expected empty string after 'operator' keyword" }
float operator "\x00" _my15 (const char *);	// { dg-error "expected empty string after 'operator' keyword" }
float operator "\h" _my16 (const char *);	// { dg-error "expected empty string after 'operator' keyword" }
						// { dg-warning "unknown escape sequence" "" { target *-*-* } .-1 }

// C++26 P2361R6 - Unevaluated strings
// { dg-do compile { target c++26 } }

static_assert (true, "foo");
static_assert (true, "foo" " " "bar");
static_assert (true, "\u01FC\U000001FC\u{1FC}\N{LATIN CAPITAL LETTER AE WITH ACUTE}\\\'\"\?\a\b\f\n\r\t\v");
static_assert (true, L"foo");		// { dg-error "a wide string is invalid in this context" }
static_assert (true, u"foo");		// { dg-error "a wide string is invalid in this context" }
static_assert (true, U"foo");		// { dg-error "a wide string is invalid in this context" }
static_assert (true, u8"foo");		// { dg-error "a wide string is invalid in this context" }
static_assert (true, L"fo" "o");	// { dg-error "a wide string is invalid in this context" }
static_assert (true, u"fo" "o");	// { dg-error "a wide string is invalid in this context" }
static_assert (true, U"fo" "o");	// { dg-error "a wide string is invalid in this context" }
static_assert (true, u8"fo" "o");	// { dg-error "a wide string is invalid in this context" }
static_assert (true, "fo" L"o");	// { dg-error "a wide string is invalid in this context" }
static_assert (true, "fo" u"o");	// { dg-error "a wide string is invalid in this context" }
static_assert (true, "fo" U"o");	// { dg-error "a wide string is invalid in this context" }
static_assert (true, "fo" u8"o");	// { dg-error "a wide string is invalid in this context" }
static_assert (true, "\0");		// { dg-error "numeric escape sequence in unevaluated string" }
static_assert (true, "\17");		// { dg-error "numeric escape sequence in unevaluated string" }
static_assert (true, "\x20");		// { dg-error "numeric escape sequence in unevaluated string" }
static_assert (true, "\o{17}");		// { dg-error "numeric escape sequence in unevaluated string" }
static_assert (true, "\x{20}");		// { dg-error "numeric escape sequence in unevaluated string" }
static_assert (true, "\h");		// { dg-error "unknown escape sequence" }

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
extern "\x43" int f11 ();		// { dg-error "numeric escape sequence in unevaluated string" }
extern "\x{43}" { int f12 (); }		// { dg-error "numeric escape sequence in unevaluated string" }
extern "\103" int f13 ();		// { dg-error "numeric escape sequence in unevaluated string" }
extern "\o{0103}" { int f14 (); }	// { dg-error "numeric escape sequence in unevaluated string" }

[[deprecated ("foo")]] int g0 ();
[[deprecated ("foo" " " "bar")]] int g1 ();
[[deprecated ("\u01FC\U000001FC\u{1FC}\N{LATIN CAPITAL LETTER AE WITH ACUTE}\\\'\"\?\a\b\f\n\r\t\v")]] int g2 ();
[[deprecated (L"foo")]] int g3 ();	// { dg-error "a wide string is invalid in this context" }
[[deprecated (u"foo")]] int g4 ();	// { dg-error "a wide string is invalid in this context" }
[[deprecated (U"foo")]] int g5 ();	// { dg-error "a wide string is invalid in this context" }
[[deprecated (u8"foo")]] int g6 ();	// { dg-error "a wide string is invalid in this context" }
[[deprecated (L"fo" "o")]] int g7 ();	// { dg-error "a wide string is invalid in this context" }
[[deprecated (u"fo" "o")]] int g8 ();	// { dg-error "a wide string is invalid in this context" }
[[deprecated (U"fo" "o")]] int g9 ();	// { dg-error "a wide string is invalid in this context" }
[[deprecated (u8"fo" "o")]] int g10 ();	// { dg-error "a wide string is invalid in this context" }
[[deprecated ("fo" L"o")]] int g11 ();	// { dg-error "a wide string is invalid in this context" }
[[deprecated ("fo" u"o")]] int g12 ();	// { dg-error "a wide string is invalid in this context" }
[[deprecated ("fo" U"o")]] int g13 ();	// { dg-error "a wide string is invalid in this context" }
[[deprecated ("fo" u8"o")]] int g14 ();	// { dg-error "a wide string is invalid in this context" }
[[deprecated ("\0")]] int g15 ();	// { dg-error "numeric escape sequence in unevaluated string" }
[[deprecated ("\17")]] int g16 ();	// { dg-error "numeric escape sequence in unevaluated string" }
[[deprecated ("\x20")]] int g17 ();	// { dg-error "numeric escape sequence in unevaluated string" }
[[deprecated ("\o{17}")]] int g18 ();	// { dg-error "numeric escape sequence in unevaluated string" }
[[deprecated ("\x{20}")]] int g19 ();	// { dg-error "numeric escape sequence in unevaluated string" }
[[deprecated ("\h")]] int g20 ();	// { dg-error "unknown escape sequence" }

[[nodiscard ("foo")]] int h0 ();
[[nodiscard ("foo" " " "bar")]] int h1 ();
[[nodiscard ("\u01FC\U000001FC\u{1FC}\N{LATIN CAPITAL LETTER AE WITH ACUTE}\\\'\"\?\a\b\f\n\r\t\v")]] int h2 ();
[[nodiscard (L"foo")]] int h3 ();	// { dg-error "a wide string is invalid in this context" }
[[nodiscard (u"foo")]] int h4 ();	// { dg-error "a wide string is invalid in this context" }
[[nodiscard (U"foo")]] int h5 ();	// { dg-error "a wide string is invalid in this context" }
[[nodiscard (u8"foo")]] int h6 ();	// { dg-error "a wide string is invalid in this context" }
[[nodiscard (L"fo" "o")]] int h7 ();	// { dg-error "a wide string is invalid in this context" }
[[nodiscard (u"fo" "o")]] int h8 ();	// { dg-error "a wide string is invalid in this context" }
[[nodiscard (U"fo" "o")]] int h9 ();	// { dg-error "a wide string is invalid in this context" }
[[nodiscard (u8"fo" "o")]] int h10 ();	// { dg-error "a wide string is invalid in this context" }
[[nodiscard ("fo" L"o")]] int h11 ();	// { dg-error "a wide string is invalid in this context" }
[[nodiscard ("fo" u"o")]] int h12 ();	// { dg-error "a wide string is invalid in this context" }
[[nodiscard ("fo" U"o")]] int h13 ();	// { dg-error "a wide string is invalid in this context" }
[[nodiscard ("fo" u8"o")]] int h14 ();	// { dg-error "a wide string is invalid in this context" }
[[nodiscard ("\0")]] int h15 ();	// { dg-error "numeric escape sequence in unevaluated string" }
[[nodiscard ("\17")]] int h16 ();	// { dg-error "numeric escape sequence in unevaluated string" }
[[nodiscard ("\x20")]] int h17 ();	// { dg-error "numeric escape sequence in unevaluated string" }
[[nodiscard ("\o{17}")]] int h18 ();	// { dg-error "numeric escape sequence in unevaluated string" }
[[nodiscard ("\x{20}")]] int h19 ();	// { dg-error "numeric escape sequence in unevaluated string" }
[[nodiscard ("\h")]] int h20 ();	// { dg-error "unknown escape sequence" }

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
float operator "" u8"" _my13 (const char *);	// { dg-error "invalid encoding prefix in literal operator" }
float operator "\0" _my14 (const char *);	// { dg-error "expected empty string after 'operator' keyword" }
float operator "\x00" _my15 (const char *);	// { dg-error "expected empty string after 'operator' keyword" }
float operator "\h" _my16 (const char *);	// { dg-error "expected empty string after 'operator' keyword" }
						// { dg-error "unknown escape sequence" "" { target *-*-* } .-1 }

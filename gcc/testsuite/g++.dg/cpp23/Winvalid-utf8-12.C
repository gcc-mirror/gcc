// P2295R6 - Support for UTF-8 as a portable source file encoding
// This test intentionally contains various byte sequences which are not valid UTF-8
// { dg-do preprocess }
// { dg-options "-finput-charset=UTF-8 -pedantic-errors -Wno-invalid-utf8" }

#define I(x)
I(߿ࠀ퟿𐀀􏿿)	// { dg-bogus "invalid UTF-8 character" }
                                // { dg-error "is not valid in an identifier" "" { target *-*-* } .-1 }
I(�)				// { dg-bogus "invalid UTF-8 character <80>" }
I(�)				// { dg-bogus "invalid UTF-8 character <bf>" }
I(�)				// { dg-bogus "invalid UTF-8 character <c0>" }
I(�)				// { dg-bogus "invalid UTF-8 character <c1>" }
I(�)				// { dg-bogus "invalid UTF-8 character <f5>" }
I(�)				// { dg-bogus "invalid UTF-8 character <ff>" }
I(�)				// { dg-bogus "invalid UTF-8 character <c2>" }
I(�)				// { dg-bogus "invalid UTF-8 character <e0>" }
I(���)				// { dg-bogus "invalid UTF-8 character <e0><80><bf>" }
I(���)				// { dg-bogus "invalid UTF-8 character <e0><9f><80>" }
I(�)				// { dg-bogus "invalid UTF-8 character <e0><bf>" }
I(�)				// { dg-bogus "invalid UTF-8 character <ec><80>" }
I(�)				// { dg-bogus "invalid UTF-8 character <ed><a0><80>" }
I(����)				// { dg-bogus "invalid UTF-8 character <f0><80><80><80>" }
I(����)				// { dg-bogus "invalid UTF-8 character <f0><8f><bf><bf>" }
I(����)				// { dg-error "is not valid in an identifier" }
I(������)			// { dg-error "is not valid in an identifier" }

/* { dg-do preprocess } */
/* { dg-options "" } */

#embed "." gnu::base64("") __gnu__::__base64__("") /* { dg-error "duplicate embed parameter 'gnu::base64'" } */
#embed __FILE__ gnu::base64 prefix() suffix() /* { dg-error "expected '\\\('" } */
#embed __FILE__ gnu::base64(1) prefix() suffix() /* { dg-error "expected character string literal" } */
#embed __FILE__ gnu::base64() prefix() suffix() /* { dg-error "expected character string literal" } */
#embed __FILE__ gnu::base64(L"SA==") /* { dg-error "expected character string literal" } */
#embed __FILE__ gnu::base64(L"S" "A==") /* { dg-error "expected character string literal" } */
/* { dg-error "expected parameter name" "" { target *-*-* } .-1 } */
#embed __FILE__ gnu::base64("S" L"A==") /* { dg-error "expected '\\\)'" } */
/* { dg-error "expected parameter name" "" { target *-*-* } .-1 } */
#embed __FILE__ gnu::base64(L"S" L"A==") /* { dg-error "expected character string literal" } */
/* { dg-error "expected parameter name" "" { target *-*-* } .-1 } */
#embed "." prefix() suffix() gnu::base64("" /* { dg-error "expected '\\\)'" } */
#embed "." gnu::base64("a") /* { dg-error "'gnu::base64' argument not valid base64 encoded string" } */
#embed "." gnu::base64("----") /* { dg-error "'gnu::base64' argument not valid base64 encoded string" } */
#embed "." gnu::base64("a===") /* { dg-error "'gnu::base64' argument not valid base64 encoded string" } */
#embed "." gnu::base64("TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2NpbmcgZWxpdCwg\nc2VkIGRvIGVpdXNtb2QgdGVtcG9yIGluY2lkaWR1bnQgdXQgbGFib3JlIGV0IGRvbG9yZSBtYWdu\nYSBhbGlxdWEuCg==") /* { dg-error "'gnu::base64' argument not valid base64 encoded string" } */
#embed "." gnu::base64("\x53\x41\x3d\x3d") /* { dg-error "'gnu::base64' argument not valid base64 encoded string" } */
#embed "." gnu::base64("\123\101\075\075") /* { dg-error "'gnu::base64' argument not valid base64 encoded string" } */
#embed "." gnu::base64("\u0053\u0041\u003d\u003d") /* { dg-error "'gnu::base64' argument not valid base64 encoded string" } */
#embed "." gnu::base64("\u{53}\u{41}\u{3d}\u{00003d}") /* { dg-error "'gnu::base64' argument not valid base64 encoded string" } */
#embed "." gnu::base64("\U00000053\U00000041\U0000003d\U0000003d") /* { dg-error "'gnu::base64' argument not valid base64 encoded string" } */
#embed "." gnu::base64("\N{LATIN CAPITAL LETTER S}\N{LATIN CAPITAL LETTER A}\N{LATIN CAPITAL LETTER A}\N{LATIN CAPITAL LETTER A}") /* { dg-error "'gnu::base64' argument not valid base64 encoded string" } */
#embed "embed-18.c" gnu::base64("SA==") /* { dg-error "'gnu::base64' parameter can be only used with '\\\".\\\"'" } */
#embed <embed-18.c> gnu::base64("SA==") /* { dg-error "'gnu::base64' parameter can be only used with '\\\".\\\"'" } */
#embed <.> gnu::base64("SA==") /* { dg-error "'gnu::base64' parameter can be only used with '\\\".\\\"'" } */
#embed "." gnu::base64("SA==") limit(3) /* { dg-error "'gnu::base64' parameter conflicts with 'limit' or 'gnu::offset' parameters" } */
#embed "." gnu::base64("SA==") gnu::offset(1) /* { dg-error "'gnu::base64' parameter conflicts with 'limit' or 'gnu::offset' parameters" } */
#if 1 + __has_embed ("." gnu::base64("") __gnu__::__base64__("")) /* { dg-error "duplicate embed parameter 'gnu::base64'" } */
#endif
#if 1 + __has_embed (__FILE__ __gnu__::__base64__ prefix() suffix()) /* { dg-error "expected '\\\('" } */
#endif
#if 1 + __has_embed (__FILE__ __gnu__::__base64__(1) prefix() suffix()) /* { dg-error "expected character string literal" } */
#endif
#if 1 + __has_embed (__FILE__ gnu::base64() prefix() suffix()) /* { dg-error "expected character string literal" } */
#endif
#if 1 + __has_embed (__FILE__ gnu::base64(L"SA==")) /* { dg-error "expected character string literal" } */
#endif
#if 1 + __has_embed (__FILE__ gnu::base64(L"S" "A==")) /* { dg-error "expected character string literal" } */
#endif /* { dg-error "missing '\\\(' in expression" "" { target *-*-* } .-1 } */
#if 1 + __has_embed (__FILE__ gnu::base64("S" L"A==")) /* { dg-error "expected '\\\)'" } */
#endif /* { dg-error "missing '\\\(' in expression" "" { target *-*-* } .-1 } */
#if 1 + __has_embed (__FILE__ gnu::base64(L"S" L"A==")) /* { dg-error "expected character string literal" } */
#endif /* { dg-error "missing '\\\(' in expression" "" { target *-*-* } .-1 } */
#if 1 + __has_embed ("." gnu::base64("a")) /* { dg-error "'gnu::base64' argument not valid base64 encoded string" } */
#endif
#if 1 + __has_embed ("." gnu::base64("----")) /* { dg-error "'gnu::base64' argument not valid base64 encoded string" } */
#endif
#if 1 + __has_embed ("." gnu::base64("a===")) /* { dg-error "'gnu::base64' argument not valid base64 encoded string" } */
#endif
#if 1 + __has_embed ("." gnu::base64("TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2NpbmcgZWxpdCwg\nc2VkIGRvIGVpdXNtb2QgdGVtcG9yIGluY2lkaWR1bnQgdXQgbGFib3JlIGV0IGRvbG9yZSBtYWdu\nYSBhbGlxdWEuCg==")) /* { dg-error "'gnu::base64' argument not valid base64 encoded string" } */
#endif

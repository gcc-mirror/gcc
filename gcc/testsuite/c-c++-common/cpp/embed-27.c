/* { dg-do preprocess { target { c || c++11 } } } */
/* { dg-options "" } */
/* { dg-additional-options "-std=gnu11" { target c } } */

#embed __FILE__ gnu::base64(u"SA==") /* { dg-error "expected character string literal" } */
#embed __FILE__ gnu::base64(u"S" "A==") /* { dg-error "expected character string literal" } */
/* { dg-error "expected parameter name" "" { target *-*-* } .-1 } */
#embed __FILE__ gnu::base64("S" u"A==") /* { dg-error "expected '\\\)'" } */
/* { dg-error "expected parameter name" "" { target *-*-* } .-1 } */
#embed __FILE__ gnu::base64(u"S" u"A==") /* { dg-error "expected character string literal" } */
/* { dg-error "expected parameter name" "" { target *-*-* } .-1 } */
#embed __FILE__ gnu::base64(U"SA==") /* { dg-error "expected character string literal" } */
#embed __FILE__ gnu::base64(U"S" "A==") /* { dg-error "expected character string literal" } */
/* { dg-error "expected parameter name" "" { target *-*-* } .-1 } */
#embed __FILE__ gnu::base64("S" U"A==") /* { dg-error "expected '\\\)'" } */
/* { dg-error "expected parameter name" "" { target *-*-* } .-1 } */
#embed __FILE__ gnu::base64(U"S" U"A==") /* { dg-error "expected character string literal" } */
/* { dg-error "expected parameter name" "" { target *-*-* } .-1 } */
#embed __FILE__ gnu::base64(u8"SA==") /* { dg-error "expected character string literal" } */
#embed __FILE__ gnu::base64(u8"S" "A==") /* { dg-error "expected character string literal" } */
/* { dg-error "expected parameter name" "" { target *-*-* } .-1 } */
#embed __FILE__ gnu::base64("S" u8"A==") /* { dg-error "expected '\\\)'" } */
/* { dg-error "expected parameter name" "" { target *-*-* } .-1 } */
#embed __FILE__ gnu::base64(u8"S" u8"A==") /* { dg-error "expected character string literal" } */
/* { dg-error "expected parameter name" "" { target *-*-* } .-1 } */
#embed "." gnu::base64(R"abc(SA==)abc") /* { dg-error "'gnu::base64' argument not valid base64" } */
#embed __FILE__ gnu::base64(LR"abc(SA==)abc") /* { dg-error "expected character string literal" } */
#embed __FILE__ gnu::base64(uR"abc(SA==)abc") /* { dg-error "expected character string literal" } */
#embed __FILE__ gnu::base64(UR"abc(SA==)abc") /* { dg-error "expected character string literal" } */
#embed __FILE__ gnu::base64(u8R"abc(SA==)abc") /* { dg-error "expected character string literal" } */
#if 1 + __has_embed (__FILE__ gnu::base64(u"SA==")) /* { dg-error "expected character string literal" } */
#endif
#if 1 + __has_embed (__FILE__ gnu::base64(u"S" "A==")) /* { dg-error "expected character string literal" } */
#endif /* { dg-error "missing '\\\(' in expression" "" { target *-*-* } .-1 } */
#if 1 + __has_embed (__FILE__ gnu::base64("S" u"A==")) /* { dg-error "expected '\\\)'" } */
#endif /* { dg-error "missing '\\\(' in expression" "" { target *-*-* } .-1 } */
#if 1 + __has_embed (__FILE__ gnu::base64(u"S" u"A==")) /* { dg-error "expected character string literal" } */
#endif /* { dg-error "missing '\\\(' in expression" "" { target *-*-* } .-1 } */
#if 1 + __has_embed (__FILE__ gnu::base64(U"SA==")) /* { dg-error "expected character string literal" } */
#endif
#if 1 + __has_embed (__FILE__ gnu::base64(U"S" "A==")) /* { dg-error "expected character string literal" } */
#endif /* { dg-error "missing '\\\(' in expression" "" { target *-*-* } .-1 } */
#if 1 + __has_embed (__FILE__ gnu::base64("S" U"A==")) /* { dg-error "expected '\\\)'" } */
#endif /* { dg-error "missing '\\\(' in expression" "" { target *-*-* } .-1 } */
#if 1 + __has_embed (__FILE__ gnu::base64(U"S" U"A==")) /* { dg-error "expected character string literal" } */
#endif /* { dg-error "missing '\\\(' in expression" "" { target *-*-* } .-1 } */
#if 1 + __has_embed (__FILE__ gnu::base64(u8"SA==")) /* { dg-error "expected character string literal" } */
#endif
#if 1 + __has_embed (__FILE__ gnu::base64(u8"S" "A==")) /* { dg-error "expected character string literal" } */
#endif /* { dg-error "missing '\\\(' in expression" "" { target *-*-* } .-1 } */
#if 1 + __has_embed (__FILE__ gnu::base64("S" u8"A==")) /* { dg-error "expected '\\\)'" } */
#endif /* { dg-error "missing '\\\(' in expression" "" { target *-*-* } .-1 } */
#if 1 + __has_embed (__FILE__ gnu::base64(u8"S" u8"A==")) /* { dg-error "expected character string literal" } */
#endif /* { dg-error "missing '\\\(' in expression" "" { target *-*-* } .-1 } */
#if 1 + __has_embed ("." gnu::base64(R"abc(SA==)abc")) /* { dg-error "'gnu::base64' argument not valid base64" } */
#endif
#if 1 + __has_embed (__FILE__ gnu::base64(LR"abc(SA==)abc")) /* { dg-error "expected character string literal" } */
#endif
#if 1 + __has_embed (__FILE__ gnu::base64(uR"abc(SA==)abc")) /* { dg-error "expected character string literal" } */
#endif
#if 1 + __has_embed (__FILE__ gnu::base64(UR"abc(SA==)abc")) /* { dg-error "expected character string literal" } */
#endif
#if 1 + __has_embed (__FILE__ gnu::base64(u8R"abc(SA==)abc")) /* { dg-error "expected character string literal" } */
#endif

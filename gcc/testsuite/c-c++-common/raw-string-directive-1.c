/* { dg-do compile } */
/* { dg-options "-std=gnu99" { target c } } */
/* { dg-options "-std=c++11" { target c++ } } */

/* Test that multi-line raw strings are lexed OK for all preprocessing
   directives where one could appear. Test raw-string-directive-2.c
   checks that #define is also processed properly.  */

/* Note that in cases where we cause GCC to produce a multi-line error
   message, we construct the string so that the second line looks enough
   like an error message for DejaGNU to process it as such, so that we
   can use dg-warning or dg-error directives to check for it.  */

#warning R"delim(line1 /* { dg-warning "line1" } */
file:15:1: warning: line2)delim" /* { dg-warning "line2" } */

#error R"delim(line3 /* { dg-error "line3" } */
file:18:1: error: line4)delim" /* { dg-error "line4" } */

#define X1 R"(line 5
line 6
line 7
line 8
/*
//
line 9)" R"delim(
line10)delim"

#define X2(a) X1 #a R"(line 11
/*
line12
)"

#if R"(line 13 /* { dg-error "line13" } */
file:35:1: error: line14)" /* { dg-error "line14\\)\"\" is not valid" } */
#endif R"(line 15 /* { dg-warning "extra tokens at end of #endif" } */
\
line16)" ""

#ifdef XYZ R"(line17 /* { dg-warning "extra tokens at end of #ifdef" } */
\
\
line18)"
#endif

#if 1
#else R"(line23 /* { dg-warning "extra tokens at end of #else" } */
\

line24)"
#endif

#if 0
#elif R"(line 25 /* { dg-error "line25" } */
file:55:1: error: line26)" /* { dg-error "line26\\)\"\" is not valid" } */
#endif

#line 60 R"(file:60:1: warning: this file has a space
in it!)"
#warning "line27" /* { dg-warning "line27" } */
/* { dg-warning "this file has a space" "#line check" { target *-*-* } 60 } */
#line 63 "file"

#undef X1 R"(line28 /* { dg-warning "extra tokens at end of #undef" } */
line29
\
)"

#ident R"(line30
line31)" R"(line 32 /* { dg-warning "extra tokens at end of #ident" } */
line 33)"

#pragma GCC diagnostic ignored R"(-Woption /* { dg-warning "-Wpragmas" } */
-with-a-newline)"

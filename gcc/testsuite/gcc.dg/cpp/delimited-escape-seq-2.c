/* P2290R3 - Delimited escape sequences */
/* { dg-do compile } */
/* { dg-require-effective-target wchar } */
/* { dg-options "-std=gnu99 -Wno-c++-compat -pedantic-errors" } */

typedef __CHAR32_TYPE__ char32_t;

const char32_t *a = U"\u{1234}";	/* { dg-error "delimited escape sequences are only valid in" } */
const char32_t *b = U"\x{1234}";	/* { dg-error "delimited escape sequences are only valid in" } */
const char32_t *c = U"\o{1234}";	/* { dg-error "delimited escape sequences are only valid in" } */

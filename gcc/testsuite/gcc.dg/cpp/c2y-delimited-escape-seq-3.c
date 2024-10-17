/* N3353 - Delimited escape sequences */
/* { dg-do compile } */
/* { dg-require-effective-target wchar } */
/* { dg-options "-std=c2y -pedantic-errors -Wno-c++-compat" } */

typedef __CHAR32_TYPE__ char32_t;

const char32_t *a = U"\u{}";				/* { dg-error "empty delimited escape sequence" } */
							/* { dg-error "is not a valid universal character" "" { target c } .-1 } */
const char32_t *b = U"\u{12" "34}";			/* { dg-error "'\\\\u\\{' not terminated with '\\}' after" } */
const char32_t *c = U"\u{0000ffffffff}";		/* { dg-error "is not a valid universal character" } */
const char32_t *d = U"\u{010000edcb}";			/* { dg-error "is not a valid universal character" } */
const char32_t *e = U"\u{02000000000000000000edcb}";	/* { dg-error "is not a valid universal character" } */
const char32_t *f = U"\u{123ghij}";			/* { dg-error "'\\\\u\\{' not terminated with '\\}' after" } */
const char32_t *g = U"\u{123.}";			/* { dg-error "'\\\\u\\{' not terminated with '\\}' after" } */
const char32_t *h = U"\u{.}";				/* { dg-error "'\\\\u\\{' not terminated with '\\}' after" } */
const char32_t *i = U"\x{}";				/* { dg-error "empty delimited escape sequence" } */
const char32_t *j = U"\x{12" "34}";			/* { dg-error "'\\\\x\\{' not terminated with '\\}' after" } */
const char32_t *k = U"\x{0000ffffffff}";
const char32_t *l = U"\x{010000edcb}";			/* { dg-error "hex escape sequence out of range" } */
const char32_t *m = U"\x{02000000000000000000edcb}";	/* { dg-error "hex escape sequence out of range" } */
const char32_t *n = U"\x{123ghij}";			/* { dg-error "'\\\\x\\{' not terminated with '\\}' after" } */
const char32_t *o = U"\x{123.}";			/* { dg-error "'\\\\x\\{' not terminated with '\\}' after" } */
const char32_t *p = U"\o{}";				/* { dg-error "empty delimited escape sequence" } */
const char32_t *q = U"\o{12" "34}";			/* { dg-error "'\\\\o\\{' not terminated with '\\}' after" } */
const char32_t *r = U"\o{0000037777777777}";
const char32_t *s = U"\o{040000166713}";		/* { dg-error "octal escape sequence out of range" } */
const char32_t *t = U"\o{02000000000000000000000166713}";/* { dg-error "octal escape sequence out of range" } */
const char32_t *u = U"\o{1238}";			/* { dg-error "'\\\\o\\{' not terminated with '\\}' after" } */
const char32_t *v = U"\o{.}";				/* { dg-error "'\\\\o\\{' not terminated with '\\}' after" } */

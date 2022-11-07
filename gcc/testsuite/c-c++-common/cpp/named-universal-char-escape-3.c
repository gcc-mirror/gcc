/* P2071R2 - Named universal character escapes */
/* { dg-do compile } */
/* { dg-require-effective-target wchar } */
/* { dg-options "-std=gnu99 -Wno-c++-compat" { target c } } */
/* { dg-options "-std=c++23" { target c++ } } */

#ifndef __cplusplus
typedef __CHAR32_TYPE__ char32_t;
#endif

const char32_t *a = U"\N{}";				/* { dg-error "empty named universal character escape sequence" } */
const char32_t *b = U"\N{NU" "LL}";			/* { dg-error "'\\\\N\\{' not terminated with '\\}' after" } */
							/* { dg-error "is not a valid universal character" "" { target c } .-1 } */
const char32_t *c = U"\N{ I've just made it up }";	/* { dg-error "'\\\\N\\{' not terminated with '\\}' after" } */
							/* { dg-error "is not a valid universal character" "" { target c } .-1 } */
const char32_t *d = U"\N{_________    _______}";	/* { dg-error "is not a valid universal character" } */
const char32_t *e = U"\N{O.X}";				/* { dg-error "'\\\\N\\{' not terminated with '\\}' after" } */
							/* { dg-error "is not a valid universal character" "" { target c } .-1 } */
const char32_t *f = U"\N{.}";				/* { dg-error "'\\\\N\\{' not terminated with '\\}' after" } */
							/* { dg-error "is not a valid universal character" "" { target c } .-1 } */
const char32_t *g = U"\N{BOM}";				/* { dg-error "is not a valid universal character" } */
const char32_t *h = U"\N{ZWNBSP}";			/* { dg-error "is not a valid universal character" } */

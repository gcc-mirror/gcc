/* P2071R2 - Named universal character escapes */
/* { dg-do compile } */
/* { dg-require-effective-target wchar } */
/* { dg-options "-std=gnu99 -Wno-c++-compat -pedantic-errors" } */

typedef __CHAR32_TYPE__ char32_t;

const char32_t *a = U"\N{ETHIOPIC SYLLABLE SEE}";	/* { dg-error "named universal character escapes are only valid in" } */

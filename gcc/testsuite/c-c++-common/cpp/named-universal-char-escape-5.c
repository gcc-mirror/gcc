/* P2071R2 - Named universal character escapes */
/* { dg-do compile } */
/* { dg-require-effective-target wchar } */
/* { dg-options "-std=gnu99 -Wno-c++-compat" { target c } } */
/* { dg-options "-std=gnu++20" { target c++ } } */

#define z(x) 0
#define a z(
int b = a\N{});				/* { dg-warning "empty named universal character escape sequence; treating it as separate tokens" } */
int c = a\N{);				/* { dg-warning "'\\\\N\\\{' not terminated with '\\\}' after \\\\N\\\{; treating it as separate tokens" } */
int d = a\N);
int e = a\NARG);
int f = a\N{abc});				/* { dg-warning "'\\\\N\\\{abc\\\}' is not a valid universal character; treating it as separate tokens" } */
int g = a\N{ABC.123});				/* { dg-warning "'\\\\N\\\{' not terminated with '\\\}' after \\\\N\\\{ABC; treating it as separate tokens" } */
int h = a\N{NON-EXISTENT CHAR});	/* { dg-warning "'\\\\N\\\{NON-EXISTENT CHAR\\\}' is not a valid universal character; treating it as separate tokens" } */
int i = a\N{Latin_Small_Letter_A_With_Acute});	/* { dg-warning "'\\\\N\\\{Latin_Small_Letter_A_With_Acute\\\}' is not a valid universal character; treating it as separate tokens" } */
					/* { dg-message "did you mean '\\\\N\\\{LATIN SMALL LETTER A WITH ACUTE\\\}'\\?" "" { target *-*-* } .-1 } */

// P2071R2 - Named universal character escapes
// { dg-do compile }
// { dg-require-effective-target wchar }

#define z(x) 0
#define a z(
int b = a\N{});				// { dg-warning "empty named universal character escape sequence; treating it as separate tokens" "" { target c++23 } }
int c = a\N{);				// { dg-warning "'\\\\N\\\{' not terminated with '\\\}' after \\\\N\\\{; treating it as separate tokens" "" { target c++23 } }
int d = a\N);
int e = a\NARG);
int f = a\N{abc});			// { dg-warning "'\\\\N\\\{abc\\\}' is not a valid universal character; treating it as separate tokens" "" { target c++23 } }
int g = a\N{ABC.123});			// { dg-warning "'\\\\N\\\{' not terminated with '\\\}' after \\\\N\\\{ABC; treating it as separate tokens" "" { target c++23 } }
int h = a\N{NON-EXISTENT CHAR});	// { dg-error "is not a valid universal character" "" { target c++23 } }
					// { dg-error "was not declared in this scope" "" { target c++23 } .-1 }
int i = a\N{Latin_Small_Letter_A_With_Acute});	// { dg-warning "'\\\\N\\\{Latin_Small_Letter_A_With_Acute\\\}' is not a valid universal character; treating it as separate tokens" "" { target c++23 } }
					// { dg-message "did you mean '\\\\N\\\{LATIN SMALL LETTER A WITH ACUTE\\\}'\\?" "" { target c++23 } .-1 }

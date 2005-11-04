// Test location of diagnostics for interpreting strings.  Bug 17964.
// Origin: Joseph Myers <joseph@codesourcery.com>
// { dg-do compile }

const char *s = "\q"; // { dg-error "unknown escape sequence" }

const char *t = "\	"; // { dg-error "unknown escape sequence" }

const char *u = "";

// Test diagnostics for interpreting strings: should be an error by
// default.
// Origin: Joseph Myers <joseph@codesourcery.com>
// { dg-do compile }
// { dg-options "" }

const char *s = "\q"; // { dg-error "error: unknown escape sequence" }

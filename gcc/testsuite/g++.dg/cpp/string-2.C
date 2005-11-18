// Test diagnostics for interpreting strings: should not be an error
// by default (4.0).
// Origin: Joseph Myers <joseph@codesourcery.com>
// { dg-do compile }
// { dg-options "" }

const char *s = "\q"; // { dg-warning "warning: unknown escape sequence" }

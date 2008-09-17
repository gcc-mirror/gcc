// Test diagnostics for interpreting strings: This is a pedwarn.
// Origin: Joseph Myers <joseph@codesourcery.com>
// { dg-do compile }
// { dg-options "" }

const char *s = "\q"; // { dg-warning "unknown escape sequence" }

/* Test that too wide bit-fields are hard errors.  PR c/3347.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk>, from PR c/3347 */
/* { dg-do compile } */
/* { dg-options "" } */

struct { int i : 1999; } x; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "width" "bit-field too wide" { target *-*-* } 6 } */

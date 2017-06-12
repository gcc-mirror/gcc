/* Test that labels at ends of compound statements are hard errors.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

void f(void) { g: } /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "label|parse|syntax" "label at end of compound statement" { target *-*-* } .-1 } */

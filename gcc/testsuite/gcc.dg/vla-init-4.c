/* Test for ICE on VLA compound literal.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

const int i = 1;
void foo() { char *p = (char [i]){ "" }; } /* { dg-error "error: compound literal has variable size" } */

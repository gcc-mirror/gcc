/* Test for ICE on incomplete-array-of-VLA compound literal.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

const int i = 1;
void foo() { void *p = (char [][i]){ "" }; } /* { dg-error "compound literal has variable size" } */

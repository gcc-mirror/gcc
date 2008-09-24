/* Test for rejection of typeof on bit-fields.  PR c/10333.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-fshow-column" } */

struct { int a:1; } x;

typeof (x.a) z; /* { dg-error "9:applied to a bit-field" "typeof" } */

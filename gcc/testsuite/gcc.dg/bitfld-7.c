/* Test for rejection of typeof on bit-fields.  PR c/10333.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

struct { int a:1; } x;

typeof (x.a) z; /* { dg-error "applied to a bit-field" "typeof" } */

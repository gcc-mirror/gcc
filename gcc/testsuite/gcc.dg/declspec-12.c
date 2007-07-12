/* Test type qualifier in empty declaration: OK but useless.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */

const struct foo; /* { dg-warning "useless type qualifier in empty declaration" } */

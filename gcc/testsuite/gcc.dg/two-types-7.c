/* { dg-do compile } */
/* { dg-options "-std=gnu89" } // suppress default -pedantic-errors */

struct s {
  struct f {}
  enum a { X } /* { dg-error "expected ';', identifier or " } */
  struct g {} /* { dg-error "expected identifier " } */
}; /* { dg-warning "no semicolon" } */

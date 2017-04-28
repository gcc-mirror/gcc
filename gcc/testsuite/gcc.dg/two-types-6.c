/* { dg-do compile } */
/* { dg-options "-std=gnu89" } // suppress default -pedantic-errors */

struct s {
  struct f {} /* dg-warning "does not declare anything" } */
  struct g {} x; /* { dg-error "expected ';', identifier or " } */
};

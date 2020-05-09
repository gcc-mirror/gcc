/* { dg-do compile } */
/* { dg-options "-std=gnu89" } // suppress default -pedantic-errors */

struct s {
  struct f {}
  struct g {} x; /* { dg-error "expected ';', identifier or " } */
};

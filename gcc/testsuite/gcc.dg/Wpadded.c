/* Source: EMC.  */

/* { dg-do compile } */
/* { dg-options "-Wpadded" } */

struct foo {
  char bar;
  long baz;                   /* { dg-warning "padding struct to align" } */
} futz;

/* Source: EMC.  */

/* { dg-do compile } */
/* -fpack-struct is necessary because the warning expected requires the initial
   packing to be larger than 1, which cannot ge guaranteed for all targets. */
/* { dg-options "-Wpadded -fpack-struct=8" } */

struct foo {
  char bar;
  long baz;                   /* { dg-warning "padding struct to align" } */
} futz;

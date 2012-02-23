/* Source: EMC.  */

/* { dg-do compile } */
/* -fpack-struct is necessary because the warning expected requires the initial
   packing to be larger than 1, which cannot be guaranteed for all targets.
   We won't get a warning anyway if the target has "packed" structure
   layout.  */
/* { dg-options "-Wpadded -fpack-struct=8" } */
/* { dg-additional-options "-mno-ms-bitfields" { target *-*-mingw* } } */

struct foo {
  char bar;
  long baz; /* { dg-warning "padding struct to align" ""  { target { ! default_packed } } } */
} futz;

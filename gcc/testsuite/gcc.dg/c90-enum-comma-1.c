/* Test for commas at end of enums: in C99 only.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

enum foo { bar, };  /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "comma at end" "enum comma error" { target *-*-* } 6 } */

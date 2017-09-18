/* PR sanitizer/81530 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined" } */

int a[(long) 4e20]; /* { dg-error "overflow in constant expression" } */
/* { dg-error "size of array .a. is too large" "" { target *-*-* } .-1 } */

/* PR sanitizer/81530 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined" } */

int a[(long) 4e20]; /* { dg-error "size of array .a. is (too large|negative)" } */

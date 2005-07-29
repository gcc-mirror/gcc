/* Test for ICE on transparent union with function pointer and
   -pedantic.  Bug 22240.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-pedantic" } */

typedef union { union w *u; int *i; } H __attribute__ ((transparent_union));
void (*h) (H);
void g (int *s) { h (s); } /* { dg-warning "ISO C prohibits argument conversion to union type" } */

/* Test the deprecated option -Wc11-c2x-compat.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -Wc11-c2x-compat" } */

void *p = nullptr; /* { dg-warning "nullptr" } */

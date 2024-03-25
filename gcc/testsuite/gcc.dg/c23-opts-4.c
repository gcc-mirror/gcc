/* Test -Wc11-c23-compat.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -Wc11-c23-compat" } */

void *p = nullptr; /* { dg-warning "nullptr" } */

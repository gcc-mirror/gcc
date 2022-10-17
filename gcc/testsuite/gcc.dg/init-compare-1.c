/* { dg-do compile } */
/* { dg-additional-options "-fdelete-null-pointer-checks" } */

extern int a, b;
int c = &a == &a;
int d = &a != &b;

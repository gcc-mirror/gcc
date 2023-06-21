/* { dg-do compile } */
/* { dg-additional-options "-fdelete-null-pointer-checks" } */
/* { dg-skip-if "" keeps_null_pointer_checks } */

extern int a, b;
int c = &a == &a;
int d = &a != &b;

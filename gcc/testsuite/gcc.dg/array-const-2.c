/* Test for array designators not integer constant expressions but
   folding to integer constants (used in Linux kernel,
   <http://gcc.gnu.org/ml/gcc/2009-04/msg00611.html>).  */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -pedantic" } */

extern int i;
int a[] = { [1 ? 1 : i] = 0 }; /* { dg-warning "array index in initializer is not an integer constant expression" } */
/* { dg-message "near initialization" "near init" { target *-*-* } .-1 } */

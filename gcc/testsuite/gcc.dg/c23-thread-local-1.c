/* Test C23 thread_local keyword.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

thread_local int a;
thread_local void f (void); /* { dg-error "storage class" } */

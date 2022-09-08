/* Test C2x thread_local keyword.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

thread_local int a;
thread_local void f (void); /* { dg-error "storage class" } */

/* Test there is no nullptr_t built-in typedef.  Bug 114869.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

int nullptr_t;

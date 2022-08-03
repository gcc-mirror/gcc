/* { dg-do compile } */
/* { dg-additional-options "-Wunused-function" } */
#pragma GCC diagnostic ignored "-Wunused-function"
static void f() {} _Pragma("GCC diagnostic error \"-Wunused-function\"") /* { dg-bogus "-Wunused-function" } */

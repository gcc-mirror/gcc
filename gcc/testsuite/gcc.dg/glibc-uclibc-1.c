/* Test -mglibc and -muclibc not allowed together.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do link { target *-*-linux* } } */
/* { dg-options "-mglibc -muclibc" } */

/* { dg-error "-mglibc and -muclibc used together" "" { target *-*-* } 0 } */

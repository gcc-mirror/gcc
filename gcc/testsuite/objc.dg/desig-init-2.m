/* Test handling of C99 designator lists in Objective-C.  Test array
   designators after structure member designators.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

struct s { int a[2]; } x = { .a[0] = 1 };
